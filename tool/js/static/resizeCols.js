//// RESIZING
//var id = "";
var dragging = false;

$('#dragbar').css("margin-left", (window.innerWidth*0.25-7) + "px");

$('#dragbar').mousedown(function(e){
    e.preventDefault();
    dragging = true;
    var main = $('#rightbar');
    var ghostbar = $('<div>',
        {id:'ghostbar',
            css: {
                height: "100%", //main.outerHeight(),
                top: "0", //main.offset().top,
                left: main.offset().left
            }
        }).appendTo('body');

    $(document).mousemove(function(e){
        ghostbar.css("left",e.pageX-2);
    });
});
$(document).mouseup(function(e){
    if (dragging)
    {
        //var percentage = ((e.pageX-2) / window.innerWidth) * 100;
        var percentage = ((e.pageX-2) / window.innerWidth) * 100;
        var mainPercentage = 100-percentage;
        // $('#console').text("side:" + percentage + " main:" + mainPercentage);

        $('#leftbar').css("width",percentage + "%");
        $('#rightbar').css("width",mainPercentage + "%");
        $('#dragbar').css("margin-left",e.pageX-9 + "px");//(percentageDB) + "%");
        $('#ghostbar').remove();
        $(document).unbind('mousemove');
        dragging = false;
    }
});