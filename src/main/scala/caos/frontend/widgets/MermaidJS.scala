package caos.frontend.widgets

object MermaidJS {
  /**
   * Make a graph for a mermaid graph specification
   * @param graph mermaid graph specification
   * @param boxId id of a div tagged with class 'mermaid' inside which the graph will be set (a container div)
   * @param svgId id of a div where the graph will be set, this div will be destroyed and replaced by an svg
   * @return
   */
  def apply(graph:String,boxId:String,svgId:String):String =
    val res = s"""
       |  var display$boxId = document.getElementById('$boxId');
       |  var text$boxId = `$graph`
       |  mermaid.render('$svgId', text$boxId).then(function(svgCode){
       |    display$boxId.innerHTML = svgCode.svg;
       |    if (svgCode.bindFunctions) svgCode.bindFunctions(display$boxId.querySelector("svg"));
       |
       |    // d3 zoom must run INSIDE the callback, since rendering is now async
       |    var svgs$boxId = d3.selectAll("#$boxId svg");
       |  	svgs$boxId.each(function () {
       |      var svg = d3.select(this);
       |    	svg.html("<g>" + svg.html() + "</g>");
       |    	var inner = svg.select("g");
       |    	var zoom = d3.zoom().on("zoom", function(event) {
       |    	  inner.attr("transform", event.transform);
       |    		});
       |    	svg.call(zoom);
       |    });
       |  }).catch(function (err) {
       |    console.error("Mermaid render failed:", err);
       |    display$boxId.textContent = "Mermaid error: " + (err && err.message ? err.message : err);
       |  });
            """.stripMargin
    //println("MermaidJS:\n"+res)
    res
    
}
