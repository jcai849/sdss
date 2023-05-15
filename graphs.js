function anim_graph(id, dots) {
    var dotIndex = 0;
    function render() {
        var dot = dots[dotIndex];
        graphviz
            .renderDot(dot)
            .on("end", function () {
                dotIndex = (dotIndex + 1) % dots.length;
                render();
            });
    }
    var graphviz = d3.select(id).graphviz()
        .transition(function () {
            return d3.transition()
                .ease(d3.easeLinear)
                .delay(2000)
                .duration(1500);
        })
        .on("initEnd", render);
}