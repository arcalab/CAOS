package caos.frontend.widgets

object Utils {
  def downloadSvg(element:String): Unit = {
    scalajs.js.eval(
      s"""svgEl = document.getElementById("$element");
         |name = "circuit.svg";
         |
         |svgEl.setAttribute("xmlns", "http://www.w3.org/2000/svg");
         |var svgData = svgEl.outerHTML;
         |
         |// Firefox, Safari root NS issue fix
         |svgData = svgData.replace('xlink=', 'xmlns:xlink=');
         |// Safari xlink NS issue fix
         |//svgData = svgData.replace(/NS\\d+:href/gi, 'xlink:href');
         |svgData = svgData.replace(/NS\\d+:href/gi, 'href');
         |// drop "stroke-dasharray: 1px, 0px;"
         |svgData = svgData.replace(/stroke-dasharray: 1px, 0px;/gi, '');
         |
         |var preface = '<?xml version="1.0" standalone="no"?>\\r\\n';
         |var svgBlob = new Blob([preface, svgData], {type:"image/svg+xml;charset=utf-8"});
         |var svgUrl = URL.createObjectURL(svgBlob);
         |var downloadLink = document.createElement("a");
         |downloadLink.href = svgUrl;
         |downloadLink.download = name;
         |document.body.appendChild(downloadLink);
         |downloadLink.click();
         |document.body.removeChild(downloadLink);
      """.stripMargin)
  }
}
