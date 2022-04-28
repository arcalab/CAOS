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
    s"""
       |  var display = document.getElementById('$boxId');
       |  var text = `$graph`
       |  var graph = mermaid.mermaidAPI.render('$svgId', text, function(svgCode){
       |    display.innerHTML = svgCode
       |    });
       |
       |  var svgs = d3.selectAll("#$boxId svg");
       |	svgs.each(function () {
       |	  var svg = d3.select(this);
       |		svg.html("<g>" + svg.html() + "</g>");
       |		var inner = svg.select("g");
       |		var zoom = d3.zoom().on("zoom", function(event) {
       |		  inner.attr("transform", event.transform);
       |			});
       |		svg.call(zoom);
       |  });
            """.stripMargin
}
