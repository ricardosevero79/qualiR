/* QualiR — interações do navegador (seleção de texto, árvores, arrastar e soltar) */
(function(){
  "use strict";
  var Q = window.QualiR = {
    sel: null,            // {start, end}
    dragSel: null, dragNode: null,
    codes: [],            // [{id,name,path,color,kind}]
    collapsedList: {}, collapsedTree: {},
    recent: []
  };
  function send(name, val){ if(window.Shiny && Shiny.setInputValue){ val = val||{}; val.nonce = Math.random(); Shiny.setInputValue(name, val, {priority:"event"}); } }
  Q.send = send;
  Q.irProjeto = function(id){ window.location.href = window.location.pathname + "?projeto=" + encodeURIComponent(id); };

  /* ------------------------ conversão seleção <-> posição ------------------------ */
  function docEl(){ return document.getElementById("doc-text"); }
  function cpLen(s){ return Array.from(s).length; }

  function pointToOffset(node, offset, isEnd){
    var d = docEl(); if(!d) return null;
    var paras = d.querySelectorAll(".para"); if(!paras.length) return null;
    var lo=0, hi=paras.length-1, idx=paras.length, r;
    while(lo<=hi){
      var mid=(lo+hi)>>1; r=document.createRange(); r.selectNodeContents(paras[mid]);
      var c; try{ c=r.comparePoint(node, offset); }catch(e){ return null; }
      if(c===1){ lo=mid+1; } else { idx=mid; hi=mid-1; }
    }
    if(idx===paras.length){ var L=paras[paras.length-1]; return (+L.dataset.start)+(+L.dataset.len); }
    var p=paras[idx]; r=document.createRange(); r.selectNodeContents(p);
    if(r.comparePoint(node, offset)===-1){
      if(isEnd && idx>0){ var q=paras[idx-1]; return (+q.dataset.start)+(+q.dataset.len); }
      return +p.dataset.start;
    }
    var r2=document.createRange(); r2.setStart(p,0); r2.setEnd(node,offset);
    return (+p.dataset.start)+cpLen(r2.toString());
  }

  function offsetToPoint(pos){
    var d=docEl(); if(!d) return null;
    var paras=d.querySelectorAll(".para");
    for(var i=0;i<paras.length;i++){
      var p=paras[i], s=+p.dataset.start, l=+p.dataset.len;
      if(pos>=s && pos<=s+l){
        var rem=pos-s, w=document.createTreeWalker(p, NodeFilter.SHOW_TEXT), t;
        while((t=w.nextNode())){
          var arr=Array.from(t.data);
          if(rem<=arr.length){ return {node:t, offset:arr.slice(0,rem).join("").length, para:p}; }
          rem-=arr.length;
        }
        return {node:p, offset:p.childNodes.length, para:p};
      }
    }
    return null;
  }

  function readSelection(){
    var d=docEl(); var s=window.getSelection();
    if(!d || !s || s.rangeCount===0 || s.isCollapsed) return null;
    var rg=s.getRangeAt(0);
    if(!d.contains(rg.commonAncestorContainer) && !rg.intersectsNode(d)) return null;
    var a=pointToOffset(rg.startContainer, rg.startOffset, false);
    var b=pointToOffset(rg.endContainer, rg.endOffset, true);
    if(a===null||b===null||b<=a) return null;
    return {start:a, end:b, text:s.toString().slice(0,200)};
  }

  function setSel(sel){
    Q.sel=sel;
    var info=document.getElementById("sel-info");
    if(info){ info.textContent = sel ? ("Seleção: "+(sel.end-sel.start)+" caracteres") : "Selecione um trecho do texto para codificar"; }
    send("doc_sel", sel?{start:sel.start,end:sel.end}:{start:-1,end:-1});
  }

  function restoreSelection(){
    if(!Q.sel) return;
    var a=offsetToPoint(Q.sel.start), b=offsetToPoint(Q.sel.end);
    if(!a||!b) return;
    try{ var r=document.createRange(); r.setStart(a.node,a.offset); r.setEnd(b.node,b.offset);
      var s=window.getSelection(); s.removeAllRanges(); s.addRange(r);}catch(e){}
  }

  /* ------------------------ menu de contexto ------------------------ */
  var ctx, ctxFilter, ctxList;
  function renderCtx(filter){
    var f=(filter||"").toLowerCase(), html="", lastKind=null, n=0;
    var items=Q.codes.filter(function(c){ return !f || c.path.toLowerCase().indexOf(f)>=0; });
    // recentes primeiro
    if(!f && Q.recent.length){
      html+='<div class="grp">RECENTES</div>';
      Q.recent.forEach(function(id){ var c=Q.codes.find(function(x){return x.id==id;}); if(c){
        html+='<div class="it" data-id="'+c.id+'"><span class="dot" style="background:'+c.color+'"></span>'+esc(c.path)+'</div>'; }});
    }
    items.forEach(function(c){
      if(c.kind!==lastKind){ html+='<div class="grp">'+(c.kind==="case"?"CASOS":"NÓS")+'</div>'; lastKind=c.kind; }
      html+='<div class="it'+(n===0&&f?' hl':'')+'" data-id="'+c.id+'"><span class="dot" style="background:'+c.color+'"></span>'+esc(c.path)+'</div>'; n++;
    });
    if(!items.length) html+='<div class="grp">nenhum nó encontrado</div>';
    ctxList.innerHTML=html;
  }
  function esc(s){ return String(s).replace(/[&<>"]/g,function(c){return {"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[c];}); }
  function showCtx(x,y){
    renderCtx(""); ctxFilter.value="";
    ctx.style.display="block";
    var w=ctx.offsetWidth, h=ctx.offsetHeight;
    ctx.style.left=Math.min(x, window.innerWidth-w-8)+"px";
    ctx.style.top=Math.min(y, window.innerHeight-h-8)+"px";
    setTimeout(function(){ ctxFilter.focus(); },10);
  }
  function hideCtx(){ if(ctx) ctx.style.display="none"; }
  function applyCode(id){
    if(!Q.sel) return;
    send("code_apply", {node:+id, start:Q.sel.start, end:Q.sel.end});
    Q.recent=[+id].concat(Q.recent.filter(function(x){return x!=id;})).slice(0,5);
    hideCtx();
  }

  /* ------------------------ colapsar listas ------------------------ */
  function applyListCollapse(table){
    var type=table.dataset.type, set=Q.collapsedList[type]||{};
    var rows=table.querySelectorAll("tr.qrow"), hideDepth=Infinity;
    rows.forEach(function(r){
      var d=+r.dataset.depth;
      if(d<=hideDepth){ hideDepth=Infinity; r.style.display=""; }
      else { r.style.display="none"; }
      r.classList.toggle("collapsed", !!set[r.dataset.id]);
      if(set[r.dataset.id] && hideDepth===Infinity) hideDepth=d;
    });
  }
  function applyTreeCollapse(root){
    root.querySelectorAll("li").forEach(function(li){
      li.classList.toggle("collapsed", !!Q.collapsedTree[li.dataset.id]);
    });
  }

  function filterList(){
    var inp=document.getElementById("list-filter"); if(!inp) return;
    var f=inp.value.toLowerCase();
    document.querySelectorAll("#listpane table.qlist").forEach(function(t){
      if(!f){ applyListCollapse(t); return; }
      t.querySelectorAll("tr.qrow").forEach(function(r){ r.style.display = r.textContent.toLowerCase().indexOf(f)>=0 ? "" : "none"; });
    });
  }

  function sortTable(th){
    var table=th.closest("table"), tb=table.tBodies[0];
    var rows=Array.from(tb.rows);
    if(rows.some(function(r){return +r.dataset.depth>0;})) return; // hierarquia: não ordenar
    var col=+th.dataset.col, asc=th.dataset.asc!=="1"; th.dataset.asc=asc?"1":"0";
    rows.sort(function(a,b){
      var x=a.cells[col].textContent.trim(), y=b.cells[col].textContent.trim();
      var nx=parseFloat(x.replace(",",".")), ny=parseFloat(y.replace(",","."));
      var c=(!isNaN(nx)&&!isNaN(ny)&&/^[\d.,%-]+$/.test(x)&&/^[\d.,%-]+$/.test(y))? nx-ny : x.localeCompare(y,"pt",{sensitivity:"base"});
      return asc?c:-c;
    });
    rows.forEach(function(r){ tb.appendChild(r); });
  }

  /* ------------------------ eventos ------------------------ */
  document.addEventListener("DOMContentLoaded", function(){
    ctx=document.createElement("div"); // substituído quando o app carregar
  });

  function bindCtx(){
    ctx=document.getElementById("qr-ctx"); if(!ctx || ctx.dataset.bound) return;
    ctx.dataset.bound="1";
    ctxFilter=document.getElementById("qr-ctx-filter"); ctxList=ctx.querySelector(".qr-ctx-list");
    ctxFilter.addEventListener("input", function(){ renderCtx(ctxFilter.value); });
    ctxFilter.addEventListener("keydown", function(e){
      if(e.key==="Enter"){ var it=ctxList.querySelector(".it.hl")||ctxList.querySelector(".it"); if(it) applyCode(it.dataset.id); e.preventDefault(); }
      if(e.key==="Escape"){ hideCtx(); }
    });
    ctxList.addEventListener("click", function(e){ var it=e.target.closest(".it"); if(it) applyCode(it.dataset.id); });
    ctx.querySelector(".qr-ctx-foot").addEventListener("click", function(e){
      var a=e.target.closest("a"); if(!a) return; e.preventDefault();
      if(!Q.sel) return;
      var nm={"new":"code_new_sel","uncode":"uncode_sel_menu","annot":"annot_sel"}[a.dataset.act];
      send(nm, {start:Q.sel.start, end:Q.sel.end});
      hideCtx();
    });
  }

  document.addEventListener("mousedown", function(e){
    if(ctx && ctx.style.display==="block" && !e.target.closest("#qr-ctx")) hideCtx();
  });

  document.addEventListener("mouseup", function(e){
    var d=docEl(); if(!d || e.button!==0) return;
    var ws=window.getSelection();
    var inDoc=d.contains(e.target) || (ws && ws.rangeCount && !ws.isCollapsed && d.contains(ws.anchorNode));
    if(!inDoc) return;
    setTimeout(function(){
      var sel=readSelection();
      if(sel){ setSel(sel); }
      else {
        var span=e.target.closest(".cod, .ann, .sc.on");
        var s=window.getSelection();
        if(span && (span.classList.contains("cod")||span.classList.contains("ann")) && s && s.rangeCount){
          var rg=s.getRangeAt(0); var pos=pointToOffset(rg.startContainer, rg.startOffset, false);
          if(pos!==null) send("seg_click", {pos:pos});
        }
        setSel(null);
      }
    }, 0);
  });

  document.addEventListener("click", function(e){
    var t=e.target;
    // abas do ribbon
    var rc=t.closest(".rb-collapse");
    if(rc){ var rb=document.getElementById("ribbon"); rb.classList.toggle("collapsed"); rb.classList.remove("peek");
      rc.innerHTML = rb.classList.contains("collapsed") ? '<i class="fas fa-angle-down"></i> Expandir' : '<i class="fas fa-angle-up"></i> Recolher'; return; }
    var rbx=document.getElementById("ribbon");
    if(rbx && rbx.classList.contains("peek") && !t.closest(".rb-tab")) rbx.classList.remove("peek");
    var tab=t.closest(".rb-tab");
    if(tab){
      if(rbx && rbx.classList.contains("collapsed")) rbx.classList.add("peek");
      document.querySelectorAll(".rb-tab").forEach(function(x){x.classList.toggle("active", x===tab);});
      document.querySelectorAll(".rb-panel").forEach(function(p){p.classList.toggle("active", p.dataset.tab===tab.dataset.tab);
        if(p.dataset.tab===tab.dataset.tab) $(p).trigger("shown");});
      return;
    }
    // painel de navegação
    var nav=t.closest(".qnav-item");
    if(nav){ e.preventDefault(); markNav(nav.dataset.sec); var lf=document.getElementById("list-filter"); if(lf) lf.value=""; send("nav", {sec:nav.dataset.sec}); return; }
    // faixa de codificação: clicar mostra o nó
    var sc=t.closest(".sc.on");
    if(sc){ send("stripe_click", {key:sc.dataset.key}); return; }
    // cabeçalho da lista: ordenar
    var th=t.closest("table.qlist th"); if(th){ sortTable(th); return; }
    // seta de colapsar
    var car=t.closest(".caret");
    if(car && !car.classList.contains("leaf")){
      var row=car.closest("tr.qrow");
      if(row){ var tb=row.closest("table"), ty=tb.dataset.type; Q.collapsedList[ty]=Q.collapsedList[ty]||{};
        Q.collapsedList[ty][row.dataset.id]=!Q.collapsedList[ty][row.dataset.id]; applyListCollapse(tb); return; }
      var li=car.closest("li");
      if(li){ Q.collapsedTree[li.dataset.id]=!Q.collapsedTree[li.dataset.id]; li.classList.toggle("collapsed"); return; }
    }
    // seleção de linha
    var r=t.closest("table.qlist tr.qrow");
    if(r){
      r.closest("tbody").querySelectorAll("tr.sel").forEach(function(x){x.classList.remove("sel");});
      r.classList.add("sel");
      send("list_sel", {type:r.dataset.type, id:r.dataset.id});
      return;
    }
    var qn=t.closest(".qnode");
    if(qn){
      document.querySelectorAll(".qnode.sel").forEach(function(x){x.classList.remove("sel");});
      qn.classList.add("sel");
      send("tree_sel", {id:+qn.dataset.id});
      return;
    }
    var pal=t.closest(".palette span");
    if(pal){ var inp=pal.closest(".modal-body").querySelector("input.qr-color"); if(inp){ inp.value=pal.dataset.c; $(inp).trigger("change"); } return; }
    var ref=t.closest("[data-goto]");
    if(ref){ e.preventDefault(); send("goto_ref", {doc:+ref.dataset.doc, start:+ref.dataset.start, end:+ref.dataset.end}); return; }
  });

  document.addEventListener("dblclick", function(e){
    var r=e.target.closest("table.qlist tr.qrow");
    if(r){ send("list_open", {type:r.dataset.type, id:r.dataset.id}); return; }
    var qn=e.target.closest(".qnode");
    if(qn){
      if(Q.sel){ applyCode(qn.dataset.id); restoreSelection(); }
      else send("list_open", {type:"node", id:qn.dataset.id});
    }
  });

  document.addEventListener("contextmenu", function(e){
    var d=docEl(); if(!d || !d.contains(e.target)) return;
    var sel=readSelection() || Q.sel;
    if(!sel) return;
    Q.sel=sel; bindCtx();
    e.preventDefault(); showCtx(e.clientX, e.clientY);
  });

  /* arrastar e soltar */
  document.addEventListener("dragstart", function(e){
    var d=docEl();
    var node=e.target.closest ? e.target.closest(".qnode, tr.qrow[data-type=node], tr.qrow[data-type=case]") : null;
    if(node){ Q.dragNode=+node.dataset.id; Q.dragSel=null; e.dataTransfer.setData("text/plain","node:"+node.dataset.id); e.dataTransfer.effectAllowed="move"; return; }
    if(d && (d.contains(e.target) || (e.target.parentNode && d.contains(e.target.parentNode)))){
      var sel=readSelection()||Q.sel;
      if(sel){ Q.sel=sel; Q.dragSel=sel; Q.dragNode=null; e.dataTransfer.effectAllowed="copy"; }
    }
  });
  function dropTarget(e){ return e.target.closest ? e.target.closest(".qnode, tr.qrow[data-type=node], tr.qrow[data-type=case], .root-drop, .drop-new") : null; }
  document.addEventListener("dragover", function(e){
    var t=dropTarget(e); if(!t) return;
    if(Q.dragSel && t.classList.contains("root-drop")) return;
    if(Q.dragNode && t.classList.contains("drop-new")) return;
    if(Q.dragSel || Q.dragNode){ e.preventDefault(); t.classList.add("drop-target"); }
  });
  document.addEventListener("dragleave", function(e){ var t=dropTarget(e); if(t) t.classList.remove("drop-target"); });
  document.addEventListener("drop", function(e){
    var t=dropTarget(e); if(!t) return;
    e.preventDefault(); t.classList.remove("drop-target");
    if(Q.dragSel && t.classList.contains("drop-new")){
      Q.sel=Q.dragSel; send("code_new_sel", {start:Q.sel.start, end:Q.sel.end});
    } else if(Q.dragSel){
      Q.sel=Q.dragSel; applyCode(t.dataset.id);
    } else if(Q.dragNode){
      var parent = t.classList.contains("root-drop") ? null : +t.dataset.id;
      if(parent!==Q.dragNode) send("node_move", {node:Q.dragNode, parent:parent, kind:t.dataset.kind||null});
    }
    Q.dragSel=null; Q.dragNode=null;
  });
  document.addEventListener("dragend", function(){ Q.dragSel=null; Q.dragNode=null;
    document.querySelectorAll(".drop-target").forEach(function(x){x.classList.remove("drop-target");}); });

  document.addEventListener("input", function(e){ if(e.target.id==="list-filter") filterList(); });
  document.addEventListener("mousedown", function(e){
    if(e.target.closest && e.target.closest("#login_go")){
      Shiny.setInputValue("login_user", document.getElementById("login_user").value);
      Shiny.setInputValue("login_pass", document.getElementById("login_pass").value);
    }
  }, true);

  document.addEventListener("keydown", function(e){
    if((e.target.id==="login_pass"||e.target.id==="login_user") && e.key==="Enter"){
      Shiny.setInputValue("login_user", document.getElementById("login_user").value);
      Shiny.setInputValue("login_pass", document.getElementById("login_pass").value);
      setTimeout(function(){ var b=document.getElementById("login_go"); if(b) b.click(); }, 50);
    }
    if(e.key==="Escape") hideCtx();
    // planilhas editáveis (DT): Enter salva a célula
    if(e.key==="Enter" && e.target.tagName==="INPUT" && e.target.closest && e.target.closest("table.dataTable td")){
      e.preventDefault(); e.target.blur();
    }
  });

  /* divisor vertical lista | detalhe */
  var drag=null;
  document.addEventListener("mousedown", function(e){
    if(e.target.id==="vsplitter"){ var lp=document.getElementById("listpane"); drag={x:e.clientX, w:lp.offsetWidth}; e.preventDefault(); }
  });
  document.addEventListener("mousemove", function(e){
    if(!drag) return;
    var lp=document.getElementById("listpane"), ws=document.getElementById("workspace");
    var w=Math.max(220, Math.min(ws.offsetWidth-320, drag.w+(e.clientX-drag.x)));
    lp.style.flex="0 0 "+w+"px";
  });
  document.addEventListener("mouseup", function(){ drag=null; });

  /* barra lateral: grupos e recolher */
  document.addEventListener("click", function(e){
    var h=e.target.closest(".sb-head");
    if(h){ h.parentNode.classList.toggle("closed"); return; }
    var tg=e.target.closest("#sb-toggle");
    if(tg){ e.preventDefault(); document.getElementById("sidebar").classList.toggle("collapsed"); return; }
    var tab=e.target.closest(".dtab");
    if(tab){
      if(e.target.closest(".x")) send("tab_close", {key:tab.dataset.key});
      else send("tab_sel", {key:tab.dataset.key});
      return;
    }
    if(e.target.closest(".dt-max")){ document.getElementById("workspace").classList.toggle("maxdetail"); return; }
  });

  /* Ctrl+Q: ir para "Codificar para" */
  document.addEventListener("keydown", function(e){
    if((e.ctrlKey||e.metaKey) && (e.key==="q"||e.key==="Q")){
      var el=document.getElementById("qc_node");
      if(el && el.selectize){ e.preventDefault(); el.selectize.focus(); }
    }
  });

  function markNav(sec){
    document.querySelectorAll(".qnav-item").forEach(function(x){
      var on=x.dataset.sec===sec; x.classList.toggle("active", on);
      if(on){ var g=x.closest(".sb-group"); if(g) g.classList.remove("closed"); }
    });
  }

  /* ------------------------ mensagens do servidor ------------------------ */
  $(document).on("shiny:connected", function(){
    Shiny.addCustomMessageHandler("qr_codes", function(m){ Q.codes=m||[]; });
    Shiny.addCustomMessageHandler("qr_nav", function(m){ markNav(m.sec); });
    Shiny.addCustomMessageHandler("qr_ctx_open", function(m){
      if(!Q.sel) return; bindCtx();
      var s=window.getSelection(), x=window.innerWidth/2, y=window.innerHeight/2;
      if(s && s.rangeCount){ var r=s.getRangeAt(0).getBoundingClientRect(); if(r.width||r.height){ x=r.left; y=r.bottom+4; } }
      showCtx(x, y);
    });
    Shiny.addCustomMessageHandler("qr_url", function(m){
      try{ history.replaceState(null, "", window.location.pathname + "?projeto=" + encodeURIComponent(m.projeto)); }catch(e){}
    });
    Shiny.addCustomMessageHandler("qr_layout", function(m){
      var ws=document.getElementById("workspace"); if(!ws) return;
      ws.classList.toggle("has-detail", !!m.open);
      if(!m.open) ws.classList.remove("maxdetail");
    });
    Shiny.addCustomMessageHandler("qr_clear_sel", function(m){ Q.sel=null; var s=window.getSelection(); if(s) s.removeAllRanges(); setSel(null); });
    Shiny.addCustomMessageHandler("qr_scroll", function(m){
      var tries=0;
      (function go(){
        var a=offsetToPoint(m.start);
        if(!a){ if(tries++<30) return setTimeout(go,150); return; }
        a.para.closest(".prow").scrollIntoView({block:"center"});
        if(m.end>m.start){
          var b=offsetToPoint(m.end);
          if(b){ try{ var r=document.createRange(); r.setStart(a.node,a.offset); r.setEnd(b.node,b.offset);
            var s=window.getSelection(); s.removeAllRanges(); s.addRange(r); Q.sel={start:m.start,end:m.end}; setSel(Q.sel);}catch(e){} }
        }
        var row=a.para.closest(".prow"); row.classList.remove("flash"); void row.offsetWidth; row.classList.add("flash");
      })();
    });
  });

  $(document).on("shiny:value", function(e){
    setTimeout(function(){
      bindCtx();
      if(e.name==="list_view"){ document.querySelectorAll("#listpane table.qlist").forEach(applyListCollapse); filterList(); }
      document.querySelectorAll(".qtree").forEach(applyTreeCollapse);
      if(e.name==="doc_html"){ restoreSelection(); }
      if(e.name==="detail_tabs"){
        var ws=document.getElementById("workspace");
        if(ws){ var open=!!document.querySelector("#detail_tabs .dtab"); ws.classList.toggle("has-detail", open); if(!open) ws.classList.remove("maxdetail"); }
      }
    }, 0);
  });

  /* entrada de cor */
  $(document).on("shiny:connected", function(){
    var b=new Shiny.InputBinding();
    $.extend(b,{
      find:function(scope){ return $(scope).find("input.qr-color"); },
      getValue:function(el){ return el.value; },
      setValue:function(el,v){ el.value=v; },
      subscribe:function(el,cb){ $(el).on("input.qrc change.qrc", function(){cb();}); },
      unsubscribe:function(el){ $(el).off(".qrc"); }
    });
    Shiny.inputBindings.register(b, "qualir.color");
  });
})();
