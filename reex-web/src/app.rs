use crate::reex::ReexComponent;
use reex::{Reex, ReexMatch};
use wasm_bindgen::JsValue;
use web_sys::HtmlInputElement;
use yew::prelude::*;

pub struct App {
    regex_input: NodeRef,
    data_input: NodeRef,
    link: ComponentLink<Self>,
    regex: String,
    data: String,
}

pub enum Msg {
    RegexUpdated(String),
    InputUpdated(String),
    Noop,
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        App {
            regex_input: Default::default(),
            data_input: Default::default(),
            link,
            regex: "hello \" \" :ahead[ world ]".to_string(),
            data: "hello world".to_string(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::RegexUpdated(regex) => {
                if self.regex != regex {
                    self.regex = regex;
                    return true;
                }
            }
            Msg::InputUpdated(input) => {
                if self.data != input {
                    self.data = input;
                    return true;
                }
            }
            Msg::Noop => {}
        }

        false
    }

    fn change(&mut self, _: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let regex_update = self
            .link
            .callback(|change: InputData| Msg::RegexUpdated(change.value));

        let data_update = self
            .link
            .callback(|change: InputData| Msg::InputUpdated(change.value));

        let mut matches = vec![];
        if let Ok(mut data) = Reex::new(&self.regex) {
            while let Some(matche) = data.find_str(&self.data) {
                web_sys::console::log_1(&JsValue::from_str(&format!("{:?}", matche)));
                matches.push(matche.data().to_string());
            }
        }

        fn render_match(matche: String) -> Html {
            html! { <span>{"｢"}{matche}{"｣"}</span> }
        };

        html! {
            <>
                <div>
                    <input ref=self.regex_input.clone() oninput=regex_update value=self.regex.clone() />
                </div>
                <div>
                    <input ref=self.data_input.clone() oninput=data_update value=self.data />
                </div>
                <div>
                    <ReexComponent input=self.regex.clone() />
                </div>

                <div>
                    { matches.len() } { "matches" }
                    { matches.into_iter().map(render_match).collect::<Html>() }
                </div>
            </>
        }
    }

    // fn rendered(&mut self, _first_render: bool) {
    //     unimplemented!()
    // }
}