import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html

pub fn layout(elements: List(Element(t))) -> Element(t) {
  html.html([attribute("lang", "en")], [
    html.head([], [
      html.title([], "Altacrypto"),
      html.meta([
        attribute("content", "width=device-width, initial-scale=1.0"),
        attribute.name("viewport"),
      ]),
      html.link([
        attribute.rel("stylesheet"),
        attribute.href(
          "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css",
        ),
      ]),
      // html.link([
    //   attribute.rel("stylesheet"),
    //   attribute.href(
    //     "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
    //   ),
    // ]),
    ]),
    html.body(
      [
        attribute.class(
          "bg-gray-100 text-gray-800 flex items-center justify-center h-screen",
        ),
      ],
      elements,
    ),
  ])
}
