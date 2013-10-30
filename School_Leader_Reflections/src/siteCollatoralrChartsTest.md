Proof of Concept using rCharts for KIPP Chicago Website
========================================================
# This is a test using Fischer's Iris Data


<link rel='stylesheet' href=http://nvd3.org/src/nv.d3.css>
<link rel='stylesheet' href=http://rawgithub.com/ramnathv/rCharts/master/inst/libraries/nvd3/css/rNVD3.css>
<script type='text/javascript' src=http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js></script>
<script type='text/javascript' src=http://d3js.org/d3.v2.min.js></script>
<script type='text/javascript' src=http://nvd3.org/nv.d3.js></script>
<script type='text/javascript' src=http://nvd3.org/lib/fisheye.js></script>


## Scatterplot with Magnifying glass from NVD3




<div id = 'testChart' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawtestChart()
    });
    function drawtestChart(){  
      var opts = {
 "dom": "testChart",
"width":    800,
"height":    400,
"x": "Sepal.Width",
"y": "Sepal.Length",
"type": "scatterChart",
"group": "Species",
"id": "testChart" 
},
        data = [
 {
 "Sepal.Length":            5.1,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.7,
"Sepal.Width":            3.2,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.6,
"Sepal.Width":            3.1,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.6,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":            3.9,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.6,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.4,
"Sepal.Width":            2.9,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":            3.1,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":            3.7,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.8,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.8,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.3,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":              4,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            4.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":            3.9,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            3.8,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.8,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.7,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.6,
"Sepal.Width":            3.6,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.3,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.8,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.2,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.2,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.7,
"Sepal.Width":            3.2,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.8,
"Sepal.Width":            3.1,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.2,
"Sepal.Width":            4.1,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            4.2,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":            3.1,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.2,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":            3.6,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.4,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.4,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.5,
"Sepal.Width":            2.3,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.4,
"Sepal.Width":            3.2,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.5,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.8,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.8,
"Sepal.Width":              3,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            3.8,
"Species": "setosa" 
},
{
 "Sepal.Length":            4.6,
"Sepal.Width":            3.2,
"Species": "setosa" 
},
{
 "Sepal.Length":            5.3,
"Sepal.Width":            3.7,
"Species": "setosa" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            3.3,
"Species": "setosa" 
},
{
 "Sepal.Length":              7,
"Sepal.Width":            3.2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            3.2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.9,
"Sepal.Width":            3.1,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            2.3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.5,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            3.3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":            2.4,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.6,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.2,
"Sepal.Width":            2.7,
"Species": "versicolor" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":              2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.9,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":            2.2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            3.1,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.7,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.2,
"Sepal.Width":            2.2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":            2.5,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.9,
"Sepal.Width":            3.2,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.5,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.6,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.8,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            2.6,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            2.4,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            2.4,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.7,
"Species": "versicolor" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":            2.7,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.4,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":            3.4,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            3.1,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            2.5,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.5,
"Sepal.Width":            2.6,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.6,
"Species": "versicolor" 
},
{
 "Sepal.Length":              5,
"Sepal.Width":            2.3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":            2.7,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":              3,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.2,
"Sepal.Width":            2.9,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.1,
"Sepal.Width":            2.5,
"Species": "versicolor" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            2.8,
"Species": "versicolor" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            3.3,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.7,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.1,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.9,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.5,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.6,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            4.9,
"Sepal.Width":            2.5,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.3,
"Sepal.Width":            2.9,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            2.5,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.2,
"Sepal.Width":            3.6,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.5,
"Sepal.Width":            3.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            2.7,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.8,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.7,
"Sepal.Width":            2.5,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            3.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.5,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.7,
"Sepal.Width":            3.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.7,
"Sepal.Width":            2.6,
"Species": "virginica" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":            2.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.9,
"Sepal.Width":            3.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.6,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.7,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.7,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            3.3,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.2,
"Sepal.Width":            3.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.2,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.2,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.4,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.9,
"Sepal.Width":            3.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.8,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.1,
"Sepal.Width":            2.6,
"Species": "virginica" 
},
{
 "Sepal.Length":            7.7,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            3.4,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.4,
"Sepal.Width":            3.1,
"Species": "virginica" 
},
{
 "Sepal.Length":              6,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.9,
"Sepal.Width":            3.1,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            3.1,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.9,
"Sepal.Width":            3.1,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.8,
"Sepal.Width":            2.7,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.8,
"Sepal.Width":            3.2,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":            3.3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.7,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.3,
"Sepal.Width":            2.5,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.5,
"Sepal.Width":              3,
"Species": "virginica" 
},
{
 "Sepal.Length":            6.2,
"Sepal.Width":            3.4,
"Species": "virginica" 
},
{
 "Sepal.Length":            5.9,
"Sepal.Width":              3,
"Species": "virginica" 
} 
]
  
      var data = d3.nest()
        .key(function(d){
          return opts.group === undefined ? 'main' : d[opts.group]
        })
        .entries(data)
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .x(function(d) { return d[opts.x] })
          .y(function(d) { return d[opts.y] })
          .width(opts.width)
          .height(opts.height)
         
        
          
        chart.xAxis
  .axisLabel("Sepal Width")

        
        
        chart.yAxis
  .axisLabel("Sepal. Length")
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>


## Histogram Plot from NVD3




A histogram with **multiBarChart**

<div id = 'nvd3Hist' class = 'rChart nvd3'></div>
<script type='text/javascript'>
 $(document).ready(function(){
      drawnvd3Hist()
    });
    function drawnvd3Hist(){  
      var opts = {
 "dom": "nvd3Hist",
"width":    800,
"height":    400,
"x": "mid",
"y": "counts",
"type": "multiBarChart",
"group": "Species",
"id": "nvd3Hist" 
},
        data = [
 {
 "mid":            4.3,
"counts": 4,
"Species": "setosa" 
},
{
 "mid":            4.5,
"counts": 5,
"Species": "setosa" 
},
{
 "mid":            4.7,
"counts": 7,
"Species": "setosa" 
},
{
 "mid":            4.9,
"counts": 12,
"Species": "setosa" 
},
{
 "mid":            5.1,
"counts": 11,
"Species": "setosa" 
},
{
 "mid":            5.3,
"counts": 6,
"Species": "setosa" 
},
{
 "mid":            5.5,
"counts": 2,
"Species": "setosa" 
},
{
 "mid":            5.7,
"counts": 3,
"Species": "setosa" 
},
{
 "mid":            5.9,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            6.1,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            6.3,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            6.5,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            6.7,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            6.9,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            7.1,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            7.3,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            7.5,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            7.7,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            7.9,
"counts": 0,
"Species": "setosa" 
},
{
 "mid":            4.3,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            4.5,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            4.7,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            4.9,
"counts": 3,
"Species": "versicolor" 
},
{
 "mid":            5.1,
"counts": 2,
"Species": "versicolor" 
},
{
 "mid":            5.3,
"counts": 1,
"Species": "versicolor" 
},
{
 "mid":            5.5,
"counts": 10,
"Species": "versicolor" 
},
{
 "mid":            5.7,
"counts": 8,
"Species": "versicolor" 
},
{
 "mid":            5.9,
"counts": 6,
"Species": "versicolor" 
},
{
 "mid":            6.1,
"counts": 6,
"Species": "versicolor" 
},
{
 "mid":            6.3,
"counts": 5,
"Species": "versicolor" 
},
{
 "mid":            6.5,
"counts": 3,
"Species": "versicolor" 
},
{
 "mid":            6.7,
"counts": 4,
"Species": "versicolor" 
},
{
 "mid":            6.9,
"counts": 2,
"Species": "versicolor" 
},
{
 "mid":            7.1,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            7.3,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            7.5,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            7.7,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            7.9,
"counts": 0,
"Species": "versicolor" 
},
{
 "mid":            4.3,
"counts": 0,
"Species": "virginica" 
},
{
 "mid":            4.5,
"counts": 0,
"Species": "virginica" 
},
{
 "mid":            4.7,
"counts": 0,
"Species": "virginica" 
},
{
 "mid":            4.9,
"counts": 1,
"Species": "virginica" 
},
{
 "mid":            5.1,
"counts": 0,
"Species": "virginica" 
},
{
 "mid":            5.3,
"counts": 0,
"Species": "virginica" 
},
{
 "mid":            5.5,
"counts": 1,
"Species": "virginica" 
},
{
 "mid":            5.7,
"counts": 4,
"Species": "virginica" 
},
{
 "mid":            5.9,
"counts": 3,
"Species": "virginica" 
},
{
 "mid":            6.1,
"counts": 4,
"Species": "virginica" 
},
{
 "mid":            6.3,
"counts": 11,
"Species": "virginica" 
},
{
 "mid":            6.5,
"counts": 4,
"Species": "virginica" 
},
{
 "mid":            6.7,
"counts": 7,
"Species": "virginica" 
},
{
 "mid":            6.9,
"counts": 3,
"Species": "virginica" 
},
{
 "mid":            7.1,
"counts": 4,
"Species": "virginica" 
},
{
 "mid":            7.3,
"counts": 2,
"Species": "virginica" 
},
{
 "mid":            7.5,
"counts": 1,
"Species": "virginica" 
},
{
 "mid":            7.7,
"counts": 4,
"Species": "virginica" 
},
{
 "mid":            7.9,
"counts": 1,
"Species": "virginica" 
} 
]
  
      var data = d3.nest()
        .key(function(d){
          return opts.group === undefined ? 'main' : d[opts.group]
        })
        .entries(data)
      
      nv.addGraph(function() {
        var chart = nv.models[opts.type]()
          .x(function(d) { return d[opts.x] })
          .y(function(d) { return d[opts.y] })
          .width(opts.width)
          .height(opts.height)
         
        chart
  .color([ "#255694", "#60A2D7", "#E27425" ])
          
        chart.xAxis
  .axisLabel("Sepal.Width")

        
        
        chart.yAxis
  .axisLabel("counts")
      
       d3.select("#" + opts.id)
        .append('svg')
        .datum(data)
        .transition().duration(500)
        .call(chart);

       nv.utils.windowResize(chart.update);
       return chart;
      });
    };
</script>

