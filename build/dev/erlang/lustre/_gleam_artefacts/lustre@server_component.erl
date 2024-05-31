-module(lustre@server_component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([component/1, script/0, route/1, data/1, include/1, subscribe/2, unsubscribe/1, emit/2, set_selector/1, decode_action/1, encode_patch/1]).

-spec component(list(lustre@internals@vdom:attribute(QTE))) -> lustre@internals@vdom:element(QTE).
component(Attrs) ->
    lustre@element:element(<<"lustre-server-component"/utf8>>, Attrs, []).

-spec script() -> lustre@internals@vdom:element(any()).
script() ->
    lustre@element:element(
        <<"script"/utf8>>,
        [lustre@attribute:attribute(<<"type"/utf8>>, <<"module"/utf8>>)],
        [lustre@element:text(
                <<"function w(t,e,i,o=!1){let s,a=[{prev:t,next:e,parent:t.parentNode}];for(;a.length;){let{prev:n,next:r,parent:l}=a.pop();if(r.subtree!==void 0&&(r=r.subtree()),r.content!==void 0)if(n)if(n.nodeType===Node.TEXT_NODE)n.textContent!==r.content&&(n.textContent=r.content),s??=n;else{let c=document.createTextNode(r.content);l.replaceChild(c,n),s??=c}else{let c=document.createTextNode(r.content);l.appendChild(c),s??=c}else if(r.tag!==void 0){let c=D({prev:n,next:r,dispatch:i,stack:a,isComponent:o});n?n!==c&&l.replaceChild(c,n):l.appendChild(c),s??=c}else r.elements!==void 0?x(r,c=>{a.unshift({prev:n,next:c,parent:l}),n=n?.nextSibling}):r.subtree!==void 0&&a.push({prev:n,next:r,parent:l})}return s}function J(t,e,i){let o=t.parentNode;for(let s of e[0]){let a=s[0].split(\"-\"),n=s[1],r=k(o,a),l;if(r!==null&&r!==o)l=w(r,n,i);else{let c=k(o,a.slice(0,-1)),u=document.createTextNode(\"\");c.appendChild(u),l=w(u,n,i)}a===\"0\"&&(t=l)}for(let s of e[1]){let a=s[0].split(\"-\");k(o,a).remove()}for(let s of e[2]){let a=s[0].split(\"-\"),n=s[1],r=k(o,a),l=N.get(r);for(let c of n[0]){let u=c[0],m=c[1];if(u.startsWith(\"data-lustre-on-\")){let b=u.slice(15),p=i(M);l.has(b)||el.addEventListener(b,y),l.set(b,p),el.setAttribute(u,m)}else r.setAttribute(u,m),r[u]=m}for(let c of n[1])if(c[0].startsWith(\"data-lustre-on-\")){let u=c[0].slice(15);r.removeEventListener(u,y),l.delete(u)}else r.removeAttribute(c[0])}return t}function D({prev:t,next:e,dispatch:i,stack:o}){let s=e.namespace||\"http://www.w3.org/1999/xhtml\",a=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),n=a?t:s?document.createElementNS(s,e.tag):document.createElement(e.tag),r;if(N.has(n))r=N.get(n);else{let d=new Map;N.set(n,d),r=d}let l=a?new Set(r.keys()):null,c=a?new Set(Array.from(t.attributes,d=>d.name)):null,u=null,m=null,b=null;for(let d of e.attrs){let f=d[0],h=d[1];if(d.as_property)n[f]!==h&&(n[f]=h),a&&c.delete(f);else if(f.startsWith(\"on\")){let g=f.slice(2),v=i(h);r.has(g)||n.addEventListener(g,y),r.set(g,v),a&&l.delete(g)}else if(f.startsWith(\"data-lustre-on-\")){let g=f.slice(15),v=i(M);r.has(g)||n.addEventListener(g,y),r.set(g,v),n.setAttribute(f,h)}else f===\"class\"?u=u===null?h:u+\" \"+h:f===\"style\"?m=m===null?h:m+h:f===\"dangerous-unescaped-html\"?b=h:(typeof h==\"string\"&&n.setAttribute(f,h),(f===\"value\"||f===\"selected\")&&(n[f]=h),a&&c.delete(f))}if(u!==null&&(n.setAttribute(\"class\",u),a&&c.delete(\"class\")),m!==null&&(n.setAttribute(\"style\",m),a&&c.delete(\"style\")),a){for(let d of c)n.removeAttribute(d);for(let d of l)r.delete(d),n.removeEventListener(d,y)}if(e.key!==void 0&&e.key!==\"\")n.setAttribute(\"data-lustre-key\",e.key);else if(b!==null)return n.innerHTML=b,n;let p=n.firstChild,E=null,C=null,T=null,A=e.children[Symbol.iterator]().next().value;a&&A!==void 0&&A.key!==void 0&&A.key!==\"\"&&(E=new Set,C=L(t),T=L(e));for(let d of e.children)x(d,f=>{f.key!==void 0&&E!==null?p=W(p,f,n,o,T,C,E):(o.unshift({prev:p,next:f,parent:n}),p=p?.nextSibling)});for(;p;){let d=p.nextSibling;n.removeChild(p),p=d}return n}var N=new WeakMap;function y(t){let e=t.currentTarget;if(!N.has(e)){e.removeEventListener(t.type,y);return}let i=N.get(e);if(!i.has(t.type)){e.removeEventListener(t.type,y);return}i.get(t.type)(t)}function M(t){let e=t.target,i=e.getAttribute(`data-lustre-on-${t.type}`),o=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),s=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":s.push(\"target.value\");break}return{tag:i,data:s.reduce((a,n)=>{let r=n.split(\".\");for(let l=0,c=a,u=t;l<r.length;l++)l===r.length-1?c[r[l]]=u[r[l]]:(c[r[l]]??={},u=u[r[l]],c=c[r[l]]);return a},{data:o})}}function L(t){let e=new Map;if(t)for(let i of t.children)x(i,o=>{let s=o?.key||o?.getAttribute?.(\"data-lustre-key\");s&&e.set(s,o)});return e}function k(t,e){let i,o,s=t;for(;[i,...o]=e,i!==void 0;)s=s.childNodes.item(i),e=o;return s}function W(t,e,i,o,s,a,n){for(;t&&!s.has(t.getAttribute(\"data-lustre-key\"));){let l=t.nextSibling;i.removeChild(t),t=l}if(a.size===0)return x(e,l=>{o.unshift({prev:t,next:l,parent:i}),t=t?.nextSibling}),t;if(n.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),o.unshift({prev:null,next:e,parent:i}),t;n.add(e.key);let r=a.get(e.key);if(!r&&!t)return o.unshift({prev:null,next:e,parent:i}),t;if(!r&&t!==null){let l=document.createTextNode(\"\");return i.insertBefore(l,t),o.unshift({prev:l,next:e,parent:i}),t}return!r||r===t?(o.unshift({prev:t,next:e,parent:i}),t=t?.nextSibling,t):(i.insertBefore(r,t),o.unshift({prev:r,next:e,parent:i}),t)}function x(t,e){if(t.elements!==void 0)for(let i of t.elements)e(i);else e(t)}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#n=null;#t=null;#e=null;constructor(){super(),this.#n=new MutationObserver(e=>{let i=[];for(let o of e)if(o.type===\"attributes\"){let{attributeName:s,oldValue:a}=o,n=this.getAttribute(s);if(a!==n)try{i.push([s,JSON.parse(n)])}catch{i.push([s,n])}}i.length&&this.#e?.send(JSON.stringify([5,i]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.appendChild(this.#t)}attributeChangedCallback(e,i,o){switch(e){case\"route\":if(!o)this.#e?.close(),this.#e=null;else if(i!==o){let s=this.getAttribute(\"id\"),a=o+(s?`?id=${s}`:\"\");this.#e?.close(),this.#e=new WebSocket(`ws://${window.location.host}${a}`),this.#e.addEventListener(\"message\",n=>this.messageReceivedCallback(n))}}}messageReceivedCallback({data:e}){let[i,...o]=JSON.parse(e);switch(i){case 0:return this.diff(o);case 1:return this.emit(o);case 2:return this.init(o)}}init([e,i]){let o=[];for(let s of e)s in this?o.push([s,this[s]]):this.hasAttribute(s)&&o.push([s,this.getAttribute(s)]),Object.defineProperty(this,s,{get(){return this[`_${s}`]??this.getAttribute(s)},set(a){let n=this[s];typeof a==\"string\"?this.setAttribute(s,a):this[`_${s}`]=a,n!==a&&this.#e?.send(JSON.stringify([5,[[s,a]]]))}});this.#n.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(i),o.length&&this.#e?.send(JSON.stringify([5,o]))}morph(e){this.#t=w(this.#t,e,i=>o=>{let s=i(o);this.#e?.send(JSON.stringify([4,s.tag,s.data]))})}diff([e]){this.#t=J(this.#t,e,i=>o=>{let s=i(o);this.#e?.send(JSON.stringify([4,s.tag,s.data]))})}emit([e,i]){this.dispatchEvent(new CustomEvent(e,{detail:i}))}disconnectedCallback(){this.#e?.close()}};window.customElements.define(\"lustre-server-component\",O);export{O as LustreServerComponent};"/utf8>>
            )]
    ).

-spec route(binary()) -> lustre@internals@vdom:attribute(any()).
route(Path) ->
    lustre@attribute:attribute(<<"route"/utf8>>, Path).

-spec data(gleam@json:json()) -> lustre@internals@vdom:attribute(any()).
data(Json) ->
    _pipe = Json,
    _pipe@1 = gleam@json:to_string(_pipe),
    lustre@attribute:attribute(<<"data-lustre-data"/utf8>>, _pipe@1).

-spec include(list(binary())) -> lustre@internals@vdom:attribute(any()).
include(Properties) ->
    _pipe = Properties,
    _pipe@1 = gleam@json:array(_pipe, fun gleam@json:string/1),
    _pipe@2 = gleam@json:to_string(_pipe@1),
    lustre@attribute:attribute(<<"data-lustre-include"/utf8>>, _pipe@2).

-spec subscribe(binary(), fun((lustre@internals@patch:patch(QTR)) -> nil)) -> lustre@internals@runtime:action(QTR, lustre:server_component()).
subscribe(Id, Renderer) ->
    {subscribe, Id, Renderer}.

-spec unsubscribe(binary()) -> lustre@internals@runtime:action(any(), lustre:server_component()).
unsubscribe(Id) ->
    {unsubscribe, Id}.

-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-spec do_set_selector(
    gleam@erlang@process:selector(lustre@internals@runtime:action(any(), QUH))
) -> lustre@effect:effect(QUH).
do_set_selector(Sel) ->
    lustre@effect:from(
        fun(_) ->
            Self = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(Self, {set_selector, Sel})
        end
    ).

-spec set_selector(
    gleam@erlang@process:selector(lustre@internals@runtime:action(any(), QUB))
) -> lustre@effect:effect(QUB).
set_selector(Sel) ->
    do_set_selector(Sel).

-spec decode_event(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_event(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple3(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Name, Data} = _use0,
            gleam@bool:guard(
                Kind /= 4,
                {error,
                    [{decode_error,
                            gleam@int:to_string(4),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        gleam@dynamic:string(Name),
                        fun(Name@1) -> {ok, {event, Name@1, Data}} end
                    )
                end
            )
        end
    ).

-spec decode_attr(gleam@dynamic:dynamic_()) -> {ok,
        {binary(), gleam@dynamic:dynamic_()}} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attr(Dyn) ->
    (gleam@dynamic:tuple2(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(Dyn).

-spec decode_attrs(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attrs(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple2(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Attrs} = _use0,
            gleam@bool:guard(
                Kind /= 5,
                {error,
                    [{decode_error,
                            gleam@int:to_string(5),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        (gleam@dynamic:list(fun decode_attr/1))(Attrs),
                        fun(Attrs@1) -> {ok, {attrs, Attrs@1}} end
                    )
                end
            )
        end
    ).

-spec decode_action(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), lustre:server_component())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_action(Dyn) ->
    (gleam@dynamic:any([fun decode_event/1, fun decode_attrs/1]))(Dyn).

-spec encode_patch(lustre@internals@patch:patch(any())) -> gleam@json:json().
encode_patch(Patch) ->
    lustre@internals@patch:patch_to_json(Patch).
