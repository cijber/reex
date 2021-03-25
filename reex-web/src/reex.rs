use reex::Reex;
use reex_ast::{ReexError, ReexItem, ReexNode};
use wasm_bindgen::JsValue;
use web_sys::Element;
use yew::utils::{document, window};
use yew::virtual_dom::{VTag, VText};
use yew::{html, Callback, Component, ComponentLink, Html, InputData, NodeRef, Properties};

#[derive(Properties, Clone, PartialEq)]
pub struct ReexProps {
    pub input: String,
    #[prop_or(Callback::noop())]
    pub onupdate: Callback<String>,
}

pub struct ReexComponent {
    data: String,
    node: Option<Result<ReexNode, ReexError>>,
    compiled: Option<Reex<char>>,
    link: ComponentLink<Self>,
    callback: Callback<String>,
    editor_ref: NodeRef,
    caret_offset: Option<u32>,
}

impl ReexComponent {
    fn parse_and_compile(&mut self, input: String) -> bool {
        web_sys::console::log_1(&JsValue::from_str(&format!("{} {}", self.data, input)));
        if input == self.data {
            return false;
        }

        if input == "" {
            self.data = input;
            self.node = None;
            self.compiled = None;

            self.callback.emit(self.data.clone());
            return true;
        }

        self.data = input;

        let res = reex_ast::ReexBuilder::parse(&self.data);
        self.node = Some(res);
        if let Some(Ok(node)) = &self.node {
            self.compiled = Some(Reex::from_node(node));
        } else {
            self.compiled = None;
        }

        web_sys::console::log_1(&JsValue::from_str(&format!("{:?}", self.node)));

        self.callback.emit(self.data.clone());

        true
    }
}

pub enum ReexMessage {
    InputUpdate(String),
    UpdateCaret,
}

impl Component for ReexComponent {
    type Message = ReexMessage;
    type Properties = ReexProps;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let mut comp = Self {
            data: "".to_string(),
            node: None,
            compiled: None,
            link,
            callback: props.onupdate,
            editor_ref: Default::default(),
            caret_offset: None,
        };

        comp.parse_and_compile(props.input);
        comp
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        match msg {
            ReexMessage::InputUpdate(input) => {
                self.update(ReexMessage::UpdateCaret);
                return self.parse_and_compile(input);
            }
            ReexMessage::UpdateCaret => {
                let document = document();
                if let Some(active) = document.active_element() {
                    if self
                        .editor_ref
                        .cast::<Element>()
                        .map_or(false, |x| x == active)
                    {
                        let selection = window().get_selection();
                        self.caret_offset =
                            selection.ok().and_then(|x| x).map(|x| x.anchor_offset());
                    } else {
                        self.caret_offset = None;
                    }
                } else {
                    self.caret_offset = None;
                }
            }
        }

        false
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        self.parse_and_compile(props.input)
    }

    fn view(&self) -> Html {
        let event = self
            .link
            .callback(move |e: InputData| ReexMessage::InputUpdate(e.value));

        fn render_node(node: &ReexNode, data: &str) -> Html {
            let mut tag = VTag::new("span");
            tag.add_attribute("data-start", &node.start());
            tag.add_attribute("data-end", &node.end());
            let mut last = node.start();

            match node.item() {
                ReexItem::Set(s) => {
                    tag.add_attribute("class", &"reex-node reex-set".to_string());
                    tag.add_attribute("data-children", &s.items().len().to_string());

                    for item in s.items() {
                        let last_new = item.start();
                        tag.add_child(VText::new(data[last..last_new].to_string()).into());
                        tag.add_child(render_node(&item, data));
                        last = item.end();
                    }
                }
                ReexItem::Options(o) => {
                    tag.add_attribute("class", &"reex-node reex-options".to_string());

                    for item in o.options() {
                        let last_new = item.start();
                        tag.add_child(VText::new(data[last..last_new].to_string()).into());
                        tag.add_child(render_node(&item, data));
                        last = item.end();
                    }
                }
                ReexItem::Flag(_) => {
                    tag.add_attribute("class", &"reex-node reex-options".to_string());
                }
                ReexItem::Block(b) => {
                    let item = b.item();
                    let last_new = item.start();
                    tag.add_child(VText::new(data[last..last_new].to_string()).into());
                    tag.add_child(render_node(&item, data));
                    last = item.end();
                }
                ReexItem::Literal(_) => {
                    tag.add_attribute("class", &"reex-node reex-literal".to_string());
                }
            }

            tag.add_child(VText::new(data[last..node.end()].to_string()).into());

            Html::VTag(Box::new(tag))
        };

        let highlight = self
            .node
            .as_ref()
            .and_then(|x| x.as_ref().ok())
            .map(|x| render_node(x, &self.data))
            .unwrap_or_else(|| VText::new(self.data.clone()).into());

        html! {
            <div class="reex-editor-container">
                <div class="reex-editor-highlight">
                    {highlight}
                </div>
                <div class="reex-editor-input" ref=self.editor_ref.clone() onfocus=self.link.callback(|_| ReexMessage::UpdateCaret) onkeyup=self.link.callback(|_| ReexMessage::UpdateCaret) onblur=self.link.callback(|_| ReexMessage::UpdateCaret) contenteditable=true oninput=event>
                    {&self.data}
                </div>
            </div>
        }
    }

    fn rendered(&mut self, _first_render: bool) {
        if let Some(offset) = self.caret_offset {
            if let Some(editor) = self.editor_ref.cast::<Element>() {
                if let Ok(Some(selection)) = window().get_selection() {
                    // let range = document().create_range();
                    // range.setStart(editor, offset);
                    // range.setEnd(editor, offset);
                    if let Some(text) = editor.first_child() {
                        // web_sys::console::log_2(&JsValue::from(offset), &JsValue::from(&text));
                        let _ = selection.collapse_with_offset(Some(&text), offset);
                    }
                }
            }
        }
    }
}
