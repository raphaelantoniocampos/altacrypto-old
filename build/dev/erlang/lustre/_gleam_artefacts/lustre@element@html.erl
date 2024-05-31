-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@internals@vdom:attribute(OSU)),
    list(lustre@internals@vdom:element(OSU))
) -> lustre@internals@vdom:element(OSU).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-spec base(list(lustre@internals@vdom:attribute(OTC))) -> lustre@internals@vdom:element(OTC).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@internals@vdom:attribute(OTG)),
    list(lustre@internals@vdom:element(OTG))
) -> lustre@internals@vdom:element(OTG).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@internals@vdom:attribute(OTM))) -> lustre@internals@vdom:element(OTM).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@internals@vdom:attribute(OTQ))) -> lustre@internals@vdom:element(OTQ).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@internals@vdom:attribute(OTU)), binary()) -> lustre@internals@vdom:element(OTU).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-spec title(list(lustre@internals@vdom:attribute(OTY)), binary()) -> lustre@internals@vdom:element(OTY).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-spec body(
    list(lustre@internals@vdom:attribute(OUC)),
    list(lustre@internals@vdom:element(OUC))
) -> lustre@internals@vdom:element(OUC).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@internals@vdom:attribute(OUI)),
    list(lustre@internals@vdom:element(OUI))
) -> lustre@internals@vdom:element(OUI).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@internals@vdom:attribute(OUO)),
    list(lustre@internals@vdom:element(OUO))
) -> lustre@internals@vdom:element(OUO).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@internals@vdom:attribute(OUU)),
    list(lustre@internals@vdom:element(OUU))
) -> lustre@internals@vdom:element(OUU).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@internals@vdom:attribute(OVA)),
    list(lustre@internals@vdom:element(OVA))
) -> lustre@internals@vdom:element(OVA).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@internals@vdom:attribute(OVG)),
    list(lustre@internals@vdom:element(OVG))
) -> lustre@internals@vdom:element(OVG).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@internals@vdom:attribute(OVM)),
    list(lustre@internals@vdom:element(OVM))
) -> lustre@internals@vdom:element(OVM).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@internals@vdom:attribute(OVS)),
    list(lustre@internals@vdom:element(OVS))
) -> lustre@internals@vdom:element(OVS).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@internals@vdom:attribute(OVY)),
    list(lustre@internals@vdom:element(OVY))
) -> lustre@internals@vdom:element(OVY).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@internals@vdom:attribute(OWE)),
    list(lustre@internals@vdom:element(OWE))
) -> lustre@internals@vdom:element(OWE).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@internals@vdom:attribute(OWK)),
    list(lustre@internals@vdom:element(OWK))
) -> lustre@internals@vdom:element(OWK).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@internals@vdom:attribute(OWQ)),
    list(lustre@internals@vdom:element(OWQ))
) -> lustre@internals@vdom:element(OWQ).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@internals@vdom:attribute(OWW)),
    list(lustre@internals@vdom:element(OWW))
) -> lustre@internals@vdom:element(OWW).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@internals@vdom:attribute(OXC)),
    list(lustre@internals@vdom:element(OXC))
) -> lustre@internals@vdom:element(OXC).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@internals@vdom:attribute(OXI)),
    list(lustre@internals@vdom:element(OXI))
) -> lustre@internals@vdom:element(OXI).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@internals@vdom:attribute(OXO)),
    list(lustre@internals@vdom:element(OXO))
) -> lustre@internals@vdom:element(OXO).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@internals@vdom:attribute(OXU)),
    list(lustre@internals@vdom:element(OXU))
) -> lustre@internals@vdom:element(OXU).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@internals@vdom:attribute(OYA)),
    list(lustre@internals@vdom:element(OYA))
) -> lustre@internals@vdom:element(OYA).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@internals@vdom:attribute(OYG)),
    list(lustre@internals@vdom:element(OYG))
) -> lustre@internals@vdom:element(OYG).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@internals@vdom:attribute(OYM)),
    list(lustre@internals@vdom:element(OYM))
) -> lustre@internals@vdom:element(OYM).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@internals@vdom:attribute(OYS)),
    list(lustre@internals@vdom:element(OYS))
) -> lustre@internals@vdom:element(OYS).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@internals@vdom:attribute(OYY)),
    list(lustre@internals@vdom:element(OYY))
) -> lustre@internals@vdom:element(OYY).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@internals@vdom:attribute(OZE)),
    list(lustre@internals@vdom:element(OZE))
) -> lustre@internals@vdom:element(OZE).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@internals@vdom:attribute(OZK)),
    list(lustre@internals@vdom:element(OZK))
) -> lustre@internals@vdom:element(OZK).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@internals@vdom:attribute(OZQ))) -> lustre@internals@vdom:element(OZQ).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@internals@vdom:attribute(OZU)),
    list(lustre@internals@vdom:element(OZU))
) -> lustre@internals@vdom:element(OZU).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@internals@vdom:attribute(PAA)),
    list(lustre@internals@vdom:element(PAA))
) -> lustre@internals@vdom:element(PAA).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@internals@vdom:attribute(PAG)),
    list(lustre@internals@vdom:element(PAG))
) -> lustre@internals@vdom:element(PAG).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@internals@vdom:attribute(PAM)),
    list(lustre@internals@vdom:element(PAM))
) -> lustre@internals@vdom:element(PAM).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@internals@vdom:attribute(PAS)),
    list(lustre@internals@vdom:element(PAS))
) -> lustre@internals@vdom:element(PAS).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@internals@vdom:attribute(PAY)),
    list(lustre@internals@vdom:element(PAY))
) -> lustre@internals@vdom:element(PAY).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@internals@vdom:attribute(PBE)),
    list(lustre@internals@vdom:element(PBE))
) -> lustre@internals@vdom:element(PBE).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@internals@vdom:attribute(PBK)),
    list(lustre@internals@vdom:element(PBK))
) -> lustre@internals@vdom:element(PBK).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@internals@vdom:attribute(PBQ)),
    list(lustre@internals@vdom:element(PBQ))
) -> lustre@internals@vdom:element(PBQ).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@internals@vdom:attribute(PBW)),
    list(lustre@internals@vdom:element(PBW))
) -> lustre@internals@vdom:element(PBW).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@internals@vdom:attribute(PCC)),
    list(lustre@internals@vdom:element(PCC))
) -> lustre@internals@vdom:element(PCC).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@internals@vdom:attribute(PCI))) -> lustre@internals@vdom:element(PCI).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@internals@vdom:attribute(PCM)),
    list(lustre@internals@vdom:element(PCM))
) -> lustre@internals@vdom:element(PCM).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@internals@vdom:attribute(PCS)),
    list(lustre@internals@vdom:element(PCS))
) -> lustre@internals@vdom:element(PCS).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@internals@vdom:attribute(PCY)),
    list(lustre@internals@vdom:element(PCY))
) -> lustre@internals@vdom:element(PCY).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@internals@vdom:attribute(PDE)),
    list(lustre@internals@vdom:element(PDE))
) -> lustre@internals@vdom:element(PDE).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@internals@vdom:attribute(PDK)),
    list(lustre@internals@vdom:element(PDK))
) -> lustre@internals@vdom:element(PDK).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@internals@vdom:attribute(PDQ)),
    list(lustre@internals@vdom:element(PDQ))
) -> lustre@internals@vdom:element(PDQ).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@internals@vdom:attribute(PDW)),
    list(lustre@internals@vdom:element(PDW))
) -> lustre@internals@vdom:element(PDW).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@internals@vdom:attribute(PEC)),
    list(lustre@internals@vdom:element(PEC))
) -> lustre@internals@vdom:element(PEC).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@internals@vdom:attribute(PEI)),
    list(lustre@internals@vdom:element(PEI))
) -> lustre@internals@vdom:element(PEI).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@internals@vdom:attribute(PEO)),
    list(lustre@internals@vdom:element(PEO))
) -> lustre@internals@vdom:element(PEO).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@internals@vdom:attribute(PEU)),
    list(lustre@internals@vdom:element(PEU))
) -> lustre@internals@vdom:element(PEU).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@internals@vdom:attribute(PFA)),
    list(lustre@internals@vdom:element(PFA))
) -> lustre@internals@vdom:element(PFA).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@internals@vdom:attribute(PFG)),
    list(lustre@internals@vdom:element(PFG))
) -> lustre@internals@vdom:element(PFG).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@internals@vdom:attribute(PFM)),
    list(lustre@internals@vdom:element(PFM))
) -> lustre@internals@vdom:element(PFM).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@internals@vdom:attribute(PFS)),
    list(lustre@internals@vdom:element(PFS))
) -> lustre@internals@vdom:element(PFS).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@internals@vdom:attribute(PFY)),
    list(lustre@internals@vdom:element(PFY))
) -> lustre@internals@vdom:element(PFY).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@internals@vdom:attribute(PGE)),
    list(lustre@internals@vdom:element(PGE))
) -> lustre@internals@vdom:element(PGE).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@internals@vdom:attribute(PGK)),
    list(lustre@internals@vdom:element(PGK))
) -> lustre@internals@vdom:element(PGK).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@internals@vdom:attribute(PGQ)),
    list(lustre@internals@vdom:element(PGQ))
) -> lustre@internals@vdom:element(PGQ).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@internals@vdom:attribute(PGW)),
    list(lustre@internals@vdom:element(PGW))
) -> lustre@internals@vdom:element(PGW).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@internals@vdom:attribute(PHC)),
    list(lustre@internals@vdom:element(PHC))
) -> lustre@internals@vdom:element(PHC).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@internals@vdom:attribute(PHI)),
    list(lustre@internals@vdom:element(PHI))
) -> lustre@internals@vdom:element(PHI).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@internals@vdom:attribute(PHO))) -> lustre@internals@vdom:element(PHO).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@internals@vdom:attribute(PHS))) -> lustre@internals@vdom:element(PHS).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@internals@vdom:attribute(PHW)),
    list(lustre@internals@vdom:element(PHW))
) -> lustre@internals@vdom:element(PHW).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@internals@vdom:attribute(PIC))) -> lustre@internals@vdom:element(PIC).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@internals@vdom:attribute(PIG)),
    list(lustre@internals@vdom:element(PIG))
) -> lustre@internals@vdom:element(PIG).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@internals@vdom:attribute(PIM))) -> lustre@internals@vdom:element(PIM).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@internals@vdom:attribute(PIQ)),
    list(lustre@internals@vdom:element(PIQ))
) -> lustre@internals@vdom:element(PIQ).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@internals@vdom:attribute(PIW))) -> lustre@internals@vdom:element(PIW).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@internals@vdom:attribute(PJA))) -> lustre@internals@vdom:element(PJA).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@internals@vdom:attribute(PJE))) -> lustre@internals@vdom:element(PJE).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@internals@vdom:attribute(PJI)),
    list(lustre@internals@vdom:element(PJI))
) -> lustre@internals@vdom:element(PJI).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@internals@vdom:attribute(PJO))) -> lustre@internals@vdom:element(PJO).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@internals@vdom:attribute(PJS))) -> lustre@internals@vdom:element(PJS).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@internals@vdom:attribute(PJW)),
    list(lustre@internals@vdom:element(PJW))
) -> lustre@internals@vdom:element(PJW).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@internals@vdom:attribute(PKC)),
    list(lustre@internals@vdom:element(PKC))
) -> lustre@internals@vdom:element(PKC).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@internals@vdom:attribute(PKI))) -> lustre@internals@vdom:element(PKI).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@internals@vdom:attribute(PKM)),
    list(lustre@internals@vdom:element(PKM))
) -> lustre@internals@vdom:element(PKM).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@internals@vdom:attribute(PKS)), binary()) -> lustre@internals@vdom:element(PKS).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-spec del(
    list(lustre@internals@vdom:attribute(PKW)),
    list(lustre@internals@vdom:element(PKW))
) -> lustre@internals@vdom:element(PKW).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@internals@vdom:attribute(PLC)),
    list(lustre@internals@vdom:element(PLC))
) -> lustre@internals@vdom:element(PLC).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@internals@vdom:attribute(PLI)),
    list(lustre@internals@vdom:element(PLI))
) -> lustre@internals@vdom:element(PLI).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@internals@vdom:attribute(PLO))) -> lustre@internals@vdom:element(PLO).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@internals@vdom:attribute(PLS)),
    list(lustre@internals@vdom:element(PLS))
) -> lustre@internals@vdom:element(PLS).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@internals@vdom:attribute(PLY)),
    list(lustre@internals@vdom:element(PLY))
) -> lustre@internals@vdom:element(PLY).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@internals@vdom:attribute(PME)),
    list(lustre@internals@vdom:element(PME))
) -> lustre@internals@vdom:element(PME).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@internals@vdom:attribute(PMK)),
    list(lustre@internals@vdom:element(PMK))
) -> lustre@internals@vdom:element(PMK).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@internals@vdom:attribute(PMQ)),
    list(lustre@internals@vdom:element(PMQ))
) -> lustre@internals@vdom:element(PMQ).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@internals@vdom:attribute(PMW)),
    list(lustre@internals@vdom:element(PMW))
) -> lustre@internals@vdom:element(PMW).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@internals@vdom:attribute(PNC)),
    list(lustre@internals@vdom:element(PNC))
) -> lustre@internals@vdom:element(PNC).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@internals@vdom:attribute(PNI)),
    list(lustre@internals@vdom:element(PNI))
) -> lustre@internals@vdom:element(PNI).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@internals@vdom:attribute(PNO)),
    list(lustre@internals@vdom:element(PNO))
) -> lustre@internals@vdom:element(PNO).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@internals@vdom:attribute(PNU)),
    list(lustre@internals@vdom:element(PNU))
) -> lustre@internals@vdom:element(PNU).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@internals@vdom:attribute(POA)),
    list(lustre@internals@vdom:element(POA))
) -> lustre@internals@vdom:element(POA).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@internals@vdom:attribute(POG)),
    list(lustre@internals@vdom:element(POG))
) -> lustre@internals@vdom:element(POG).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@internals@vdom:attribute(POM))) -> lustre@internals@vdom:element(POM).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@internals@vdom:attribute(POQ)),
    list(lustre@internals@vdom:element(POQ))
) -> lustre@internals@vdom:element(POQ).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@internals@vdom:attribute(POW)),
    list(lustre@internals@vdom:element(POW))
) -> lustre@internals@vdom:element(POW).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@internals@vdom:attribute(PPC)),
    list(lustre@internals@vdom:element(PPC))
) -> lustre@internals@vdom:element(PPC).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@internals@vdom:attribute(PPI)),
    list(lustre@internals@vdom:element(PPI))
) -> lustre@internals@vdom:element(PPI).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@internals@vdom:attribute(PPO)), binary()) -> lustre@internals@vdom:element(PPO).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-spec output(
    list(lustre@internals@vdom:attribute(PPS)),
    list(lustre@internals@vdom:element(PPS))
) -> lustre@internals@vdom:element(PPS).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@internals@vdom:attribute(PPY)),
    list(lustre@internals@vdom:element(PPY))
) -> lustre@internals@vdom:element(PPY).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@internals@vdom:attribute(PQE)),
    list(lustre@internals@vdom:element(PQE))
) -> lustre@internals@vdom:element(PQE).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@internals@vdom:attribute(PQK)), binary()) -> lustre@internals@vdom:element(PQK).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec details(
    list(lustre@internals@vdom:attribute(PQO)),
    list(lustre@internals@vdom:element(PQO))
) -> lustre@internals@vdom:element(PQO).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@internals@vdom:attribute(PQU)),
    list(lustre@internals@vdom:element(PQU))
) -> lustre@internals@vdom:element(PQU).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@internals@vdom:attribute(PRA)),
    list(lustre@internals@vdom:element(PRA))
) -> lustre@internals@vdom:element(PRA).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@internals@vdom:attribute(PRG))) -> lustre@internals@vdom:element(PRG).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@internals@vdom:attribute(PRK)),
    list(lustre@internals@vdom:element(PRK))
) -> lustre@internals@vdom:element(PRK).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
