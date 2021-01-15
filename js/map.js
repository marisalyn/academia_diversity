var clicked = "overall";
var clicked_name = "none";
function buildMap(containerId) {
  var margin = {
    top: 50,
    right: 80,
    bottom: 50,
    left: 80
  };

  // calculate dimensions
  var width = 1300;
  var height = 750;
  var innerWidth = width - margin.left - margin.right;
  var innerHeight = height - margin.top - margin.bottom;

  function handleError(error, msg) {
    if (error) {
        console.error(msg);
    }
  }
  
  // create svg element
  var svg = d3.select(containerId)
      .append("svg")
      .attr("height", innerHeight)
      .attr("width", innerWidth)
      .attr("transform", 
      "translate(" + margin.left + "," + margin.top + ")");
  
  // define path generator with albers USA projection
  var projection = d3.geoAlbersUsa();

  var path = d3.geoPath().projection(projection);
  
  function showTooltip(selection){
         selection
           .raise()
           .style("fill", "#5B5F97")
           .style("opacity", 1)
           .style("stroke", "black")
           .style("stroke-width", "0.6px");
  
          tooltip.transition() 
                 .duration(500)
                 .style("opacity", 1);
      }
  
  function hideTooltip(selection){
      selection
        .style("fill", "#FFC145")
        .style("opacity", 0.8)
        .style("stroke", "black")
        .style("stroke-width", "0.6px");
       
      tooltip.transition()
             .duration(500)
             .style("opacity", 0);
  }
    
  //define div for tooltip 
  var tooltip = d3.select("body")
                  .append("div")
                  .attr("class", "tooltip")
                  .style("opacity", 0); 
  
  // read in our data
  d3.json('data/processed/us.json', function(error, json) {
    handleError(error, 'failed to read  county data');
    d3.csv('data/processed/uni_locs.csv', function(error, data) {
      handleError(error, 'failed to read uni data');
      data.forEach(function(d) {
        d.uni_id = d.uni_id;
        d.longitude = +d.longitude;
        d.latitude = +d.latitude;
        d.uni = d.uni
      });

      svg.append("g")
       .selectAll("path")
       .data(json.features)
       .enter()
       .append("path")
       .attr("d", path)
       .style("fill", "#FFFFFF")
       .style("stroke", "#000000")
       .style("stroke-width", 2);
      
      var map = svg.selectAll("circle")
        .data(data)
        .enter()
        .append("circle")
        .attr("cx", function(d) {if(projection([d.longitude, d.latitude])) {
          return projection([d.longitude, d.latitude])[0];
          } else {
            //console.log("not plotting", d.uni)
            return;
          }})
        .attr("cy", function(d) {if(projection([d.longitude, d.latitude])){
          return projection([d.longitude, d.latitude])[1];
          } else{
            return;
          }})
        .attr("r", 4)
        .style("fill", "#FFC145")
        .style("opacity", 0)
        .style("stroke", "black")
        .style("stroke-width", "0.6px");
      
      map.transition()
        .delay(function(d) {if(projection([d.longitude, d.latitude])) {
          return projection([d.longitude, d.latitude])[0] * 2;
          } else {
            return;
          }})
        .style("opacity", 0.8);
      
       
       map
         .on("mouseover", function(d){
         tooltip.html(d.uni) 
                .style("fill", "#5B5F97")
                .style("opacity", 1)
                .style("left", d3.event.pageX + 20 + "px")
                .style("top", d3.event.pageY + 20 + "px");
       d3.select(this).call(showTooltip);
      })
      .on("mouseout", function(){
         d3.select(this).call(hideTooltip);
      })
      .on("click", function(d) {
        clicked = d.uni_id;
        clicked_name = d.uni;
        console.log("updating to uni :", d.uni)
      });
      return(clicked)
    });
  });
}

buildMap("#map")
