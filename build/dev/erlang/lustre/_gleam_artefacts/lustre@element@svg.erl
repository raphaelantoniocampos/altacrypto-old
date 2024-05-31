-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-spec animate(list(lustre@internals@vdom:attribute(PYN))) -> lustre@internals@vdom:element(PYN).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-spec animate_motion(list(lustre@internals@vdom:attribute(PYR))) -> lustre@internals@vdom:element(PYR).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-spec animate_transform(list(lustre@internals@vdom:attribute(PYV))) -> lustre@internals@vdom:element(PYV).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-spec mpath(list(lustre@internals@vdom:attribute(PYZ))) -> lustre@internals@vdom:element(PYZ).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-spec set(list(lustre@internals@vdom:attribute(PZD))) -> lustre@internals@vdom:element(PZD).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-spec circle(list(lustre@internals@vdom:attribute(PZH))) -> lustre@internals@vdom:element(PZH).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-spec ellipse(list(lustre@internals@vdom:attribute(PZL))) -> lustre@internals@vdom:element(PZL).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-spec line(list(lustre@internals@vdom:attribute(PZP))) -> lustre@internals@vdom:element(PZP).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-spec polygon(list(lustre@internals@vdom:attribute(PZT))) -> lustre@internals@vdom:element(PZT).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-spec polyline(list(lustre@internals@vdom:attribute(PZX))) -> lustre@internals@vdom:element(PZX).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-spec rect(list(lustre@internals@vdom:attribute(QAB))) -> lustre@internals@vdom:element(QAB).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-spec a(
    list(lustre@internals@vdom:attribute(QAF)),
    list(lustre@internals@vdom:element(QAF))
) -> lustre@internals@vdom:element(QAF).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-spec defs(
    list(lustre@internals@vdom:attribute(QAL)),
    list(lustre@internals@vdom:element(QAL))
) -> lustre@internals@vdom:element(QAL).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-spec g(
    list(lustre@internals@vdom:attribute(QAR)),
    list(lustre@internals@vdom:element(QAR))
) -> lustre@internals@vdom:element(QAR).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-spec marker(
    list(lustre@internals@vdom:attribute(QAX)),
    list(lustre@internals@vdom:element(QAX))
) -> lustre@internals@vdom:element(QAX).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-spec mask(
    list(lustre@internals@vdom:attribute(QBD)),
    list(lustre@internals@vdom:element(QBD))
) -> lustre@internals@vdom:element(QBD).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-spec missing_glyph(
    list(lustre@internals@vdom:attribute(QBJ)),
    list(lustre@internals@vdom:element(QBJ))
) -> lustre@internals@vdom:element(QBJ).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-spec pattern(
    list(lustre@internals@vdom:attribute(QBP)),
    list(lustre@internals@vdom:element(QBP))
) -> lustre@internals@vdom:element(QBP).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-spec svg(
    list(lustre@internals@vdom:attribute(QBV)),
    list(lustre@internals@vdom:element(QBV))
) -> lustre@internals@vdom:element(QBV).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec switch(
    list(lustre@internals@vdom:attribute(QCB)),
    list(lustre@internals@vdom:element(QCB))
) -> lustre@internals@vdom:element(QCB).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-spec symbol(
    list(lustre@internals@vdom:attribute(QCH)),
    list(lustre@internals@vdom:element(QCH))
) -> lustre@internals@vdom:element(QCH).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-spec desc(
    list(lustre@internals@vdom:attribute(QCN)),
    list(lustre@internals@vdom:element(QCN))
) -> lustre@internals@vdom:element(QCN).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-spec metadata(
    list(lustre@internals@vdom:attribute(QCT)),
    list(lustre@internals@vdom:element(QCT))
) -> lustre@internals@vdom:element(QCT).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-spec title(
    list(lustre@internals@vdom:attribute(QCZ)),
    list(lustre@internals@vdom:element(QCZ))
) -> lustre@internals@vdom:element(QCZ).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_blend(list(lustre@internals@vdom:attribute(QDF))) -> lustre@internals@vdom:element(QDF).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-spec fe_color_matrix(list(lustre@internals@vdom:attribute(QDJ))) -> lustre@internals@vdom:element(QDJ).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_component_transfer(list(lustre@internals@vdom:attribute(QDN))) -> lustre@internals@vdom:element(QDN).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-spec fe_composite(list(lustre@internals@vdom:attribute(QDR))) -> lustre@internals@vdom:element(QDR).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(QDV))) -> lustre@internals@vdom:element(QDV).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(QDZ)),
    list(lustre@internals@vdom:element(QDZ))
) -> lustre@internals@vdom:element(QDZ).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_displacement_map(list(lustre@internals@vdom:attribute(QEF))) -> lustre@internals@vdom:element(QEF).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(QEJ))) -> lustre@internals@vdom:element(QEJ).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-spec fe_flood(list(lustre@internals@vdom:attribute(QEN))) -> lustre@internals@vdom:element(QEN).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_a(list(lustre@internals@vdom:attribute(QER))) -> lustre@internals@vdom:element(QER).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_b(list(lustre@internals@vdom:attribute(QEV))) -> lustre@internals@vdom:element(QEV).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_g(list(lustre@internals@vdom:attribute(QEZ))) -> lustre@internals@vdom:element(QEZ).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_r(list(lustre@internals@vdom:attribute(QFD))) -> lustre@internals@vdom:element(QFD).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(QFH))) -> lustre@internals@vdom:element(QFH).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-spec fe_image(list(lustre@internals@vdom:attribute(QFL))) -> lustre@internals@vdom:element(QFL).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-spec fe_merge(
    list(lustre@internals@vdom:attribute(QFP)),
    list(lustre@internals@vdom:element(QFP))
) -> lustre@internals@vdom:element(QFP).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_merge_node(list(lustre@internals@vdom:attribute(QFV))) -> lustre@internals@vdom:element(QFV).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-spec fe_morphology(list(lustre@internals@vdom:attribute(QFZ))) -> lustre@internals@vdom:element(QFZ).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-spec fe_offset(list(lustre@internals@vdom:attribute(QGD))) -> lustre@internals@vdom:element(QGD).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(QGH)),
    list(lustre@internals@vdom:element(QGH))
) -> lustre@internals@vdom:element(QGH).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_tile(
    list(lustre@internals@vdom:attribute(QGN)),
    list(lustre@internals@vdom:element(QGN))
) -> lustre@internals@vdom:element(QGN).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_turbulence(list(lustre@internals@vdom:attribute(QGT))) -> lustre@internals@vdom:element(QGT).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-spec linear_gradient(
    list(lustre@internals@vdom:attribute(QGX)),
    list(lustre@internals@vdom:element(QGX))
) -> lustre@internals@vdom:element(QGX).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec radial_gradient(
    list(lustre@internals@vdom:attribute(QHD)),
    list(lustre@internals@vdom:element(QHD))
) -> lustre@internals@vdom:element(QHD).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec stop(list(lustre@internals@vdom:attribute(QHJ))) -> lustre@internals@vdom:element(QHJ).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-spec image(list(lustre@internals@vdom:attribute(QHN))) -> lustre@internals@vdom:element(QHN).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-spec path(list(lustre@internals@vdom:attribute(QHR))) -> lustre@internals@vdom:element(QHR).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-spec text(list(lustre@internals@vdom:attribute(QHV)), binary()) -> lustre@internals@vdom:element(QHV).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec use_(list(lustre@internals@vdom:attribute(QHZ))) -> lustre@internals@vdom:element(QHZ).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-spec fe_distant_light(list(lustre@internals@vdom:attribute(QID))) -> lustre@internals@vdom:element(QID).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_point_light(list(lustre@internals@vdom:attribute(QIH))) -> lustre@internals@vdom:element(QIH).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_spot_light(list(lustre@internals@vdom:attribute(QIL))) -> lustre@internals@vdom:element(QIL).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-spec clip_path(
    list(lustre@internals@vdom:attribute(QIP)),
    list(lustre@internals@vdom:element(QIP))
) -> lustre@internals@vdom:element(QIP).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-spec script(list(lustre@internals@vdom:attribute(QIV)), binary()) -> lustre@internals@vdom:element(QIV).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-spec style(list(lustre@internals@vdom:attribute(QIZ)), binary()) -> lustre@internals@vdom:element(QIZ).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-spec foreign_object(
    list(lustre@internals@vdom:attribute(QJD)),
    list(lustre@internals@vdom:element(QJD))
) -> lustre@internals@vdom:element(QJD).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-spec text_path(
    list(lustre@internals@vdom:attribute(QJJ)),
    list(lustre@internals@vdom:element(QJJ))
) -> lustre@internals@vdom:element(QJJ).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-spec tspan(
    list(lustre@internals@vdom:attribute(QJP)),
    list(lustre@internals@vdom:element(QJP))
) -> lustre@internals@vdom:element(QJP).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
