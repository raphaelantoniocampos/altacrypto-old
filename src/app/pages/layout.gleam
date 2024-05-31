import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn layout(elements: List(Element(t))) -> Element(t) {
  html.html([], [
    html.head([], [
      html.title([], "Altacrypto"),
      html.meta([
        attribute.name("viewport"),
        attribute.attribute("content", "width=device-width, initial-scale=1"),
      ]),
      html.link([attribute.rel("stylesheet"), attribute.href("/static/app.css")]),
      // html.link([
    //   attribute.rel("stylesheet"),
    //   attribute.href(
    //     "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
    //   ),
    // ]),
    ]),
    html.body([], elements),
  ])
}
