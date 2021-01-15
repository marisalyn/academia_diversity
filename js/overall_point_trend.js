var margin_dots = {
    top: 50,
    right: 80,
    bottom: 75,
    left: 180
};

// calculate dimensions
var width_dots = 1000;
var height_dots = 225;
var innerWidth_dots = width_dots - margin_dots.left - margin_dots.right;
var innerHeight_dots = height_dots - margin_dots.top - margin_dots.bottom;

//define div for tooltip 
var tooltip_dots = d3.select("body")
  .append("div")
  .attr("class", "tooltip")
  .style("opacity", 0); 

var color_dots = d3.scaleOrdinal()
    .range(["#ffa600",
            "#ff7c43",
            "#f95d6a",
            "#d45087",
            "#a05195",
            "#665191",
            "#2f4b7c",
            "#003f5c",
            "#808080"
            ]); 

var y_dots = d3.scaleBand()
    .rangeRound([0, innerHeight_dots])
    .paddingInner(0.15);

var x_dots = d3.scaleLinear()
    .rangeRound([0, innerWidth_dots])  

var xAxis_dots = d3.axisBottom(x_dots).ticks(4).tickSizeOuter(0)
var yAxis_dots = d3.axisLeft(y_dots).ticks(3).tickSizeOuter(0);

function buildDots(containerId, race) {
    // read in our data
    d3.csv('data/processed/overall_race.csv',  function(error, data) {
        handleError(error, 'failed to read data');
        // create svg element
        var dots = d3.select(containerId)
            .append("svg")
            .attr("height", height_dots)
            .attr("width", width_dots)
            .append("g")
            .attr("transform", "translate(" + margin_dots.left + "," + margin_dots.top + ")");

         // filter to group of interest
         var races = d3.nest()
            .key(function(d) { return d.race; })
            .entries(data.filter(d => d.year == 2018))
            .map(function(d) { return d.key; });

         var data = data.filter(d => d.race === race && 
            (d.year == 2012 || d.year == 2018) &&
            (d.group != "U.S. population"))

        var groups = d3.nest()
            .key(function(d) { return d.group; })
            .entries(data)
            .map(function(d) { return d.key; });

         data.forEach(function(d) {
            d.year = +d.year;
            d.group = d.group;
            d.n = +d.n;
            d.percent = +d.percent;
            d.percent_diff = +d.percent_diff;
        });

        // restructure data
        var nested_data = d3.nest()
            .key(function(d) {return d.group})
            .entries(data);

        console.log("nested", nested_data)
        x_dots.domain([
            d3.min(data, function(d) {return d.percent;})*0.9,
            d3.max(data, function(d) {return d.percent;})*1.1 
        ]);
        
        y_dots.domain(groups)
        color_dots.domain(races)
        
          // create dot chart 
            var addLine = d3.line()
                .x(function(d) { return x_dots(d.percent) ; }) // check
                .y(function(d) { return y_dots(d.group) + y_dots.bandwidth()/2; })
            
            dots.append("defs").append("marker")
                .attr("id", "arrowhead")
                .attr("refX", 10) 
                .attr("refY", 5)
                .attr("fill", "#808080")
                .attr("markerWidth", 10)
                .attr("markerHeight", 50)
                .attr("orient", "auto")
                .append("path")
                    .attr("d", "M 0 0 L 4 5 L 0 10 z"); //M 0,0 V 4 L6,2 Z // M 0 0 L 10 5 L 0 10 z

            dots.selectAll('line')
                .data(nested_data)
                .enter().append("path")
                .attr("marker-end", "url(#arrowhead)")
                .attr("d", function(d){ return addLine(d.values)})
                .attr("stroke", "#808080")
                .attr("stroke-width", "1.5")          
                .attr("fill", "transparent")
                .attr("class", "edges");
                   
            dots.selectAll("g")
                .data(data)
                .enter().append("circle")
                  .attr("cx", function (d) { return x_dots(d.percent); } )
                  .attr("cy", function (d) { return y_dots(d.group) +  y_dots.bandwidth()/2; } )
                  .attr("r", 5)
                  .style("stroke", function(d) {return d.year === 2012 ? "#808080": "#000000" })
                  .style("stroke-width", function(d) {return d.year === 2012 ? 1.5: 1 })
                  .style("fill", function(d) {return d.year === 2012 ? "#FFFFFF": color_dots(d.race) })
                  .attr("class", "dots")
                .on("mouseover", function(d) { 
                   tooltip_dots.html(d.year + " " + d.group +
                   "<br>" + 
                   d3.format(".2%")(d.percent))
                     .style("left", d3.event.pageX  + 20 + "px")
                     .style("top", d3.event.pageY + 20+ "px")
                     .style("opacity", 1); })
               .on("mouseout", function() {return tooltip_dots.style("opacity", 0);})
                 
            // add axes
            dots.append('g')
                .attr('class', 'xaxis')
                .attr('transform', 'translate(0,' + innerHeight_dots + ')')
                .call(xAxis_dots.tickFormat(d3.format(".0%"))) 
                .selectAll("text")	
                    .style("text-anchor", "middle")
                    .attr("dy", "15px");
    
            // add title
            dots.append('text')
               .attr("class", "title")
               .attr("x", innerWidth_dots / 2)
               .attr("y", -25)
               .attr("text-anchor", 'middle')
               .text(race);
  
            dots.append('g')
                .attr('class', 'yaxis')
                .call(yAxis_dots);
            
    });
}

buildDots('#overall_trend_w', "White");
buildDots('#overall_trend_b', "Black or African American");
buildDots('#overall_trend_h', "Hispanic or Latino");
buildDots('#overall_trend_a', "Asian");
buildDots('#overall_trend_n', "American Indian and Alaska Native");
buildDots('#overall_trend_p', "Native Hawaiian and Other Pacific Islander");
buildDots('#overall_trend_t', "Two or more races");

function updateDots(containerId, race, selected_uni, selected_uni_name) {
      // read in our data
      d3.csv('data/processed/all_race_2012_2018.csv',  function(error, data) {
        data.forEach(function(d) {
            d.year = +d.year;
            d.group = d.group;
            d.n = +d.n;
            d.percent = +d.percent;
        });
           // filter to group of interest
        var races = d3.nest()
           .key(function(d) { return d.race; })
           .entries(data.filter(d => d.year == 2018))
           .map(function(d) { return d.key; });
            
        data = data.filter(d => d.race == race && 
          d.uni_id == selected_uni && 
          d.group != "U.S. population")
 
        var groups = d3.nest()
            .key(function(d) { return d.group; })
            .entries(data)
            .map(function(d) { return d.key; });
        
          // restructure data
          var nested_data = d3.nest()
                              .key(function(d) {return d.group})
                              .entries(data);
  
          console.log("nested uni ", nested_data)

          // set x, y and colors
        x_dots
          .domain([
              d3.min(data, function(d) {return d.percent;})*0.9,
              d3.max(data, function(d) {return d.percent;})*1.1 
        ]);
        
        y_dots.domain(groups)
        color_dots.domain(races)
          
        // create dot chart 
        addLine = d3.line()
                  .x(function(d) { return x_dots(d.percent) ; }) //??
                  .y(function(d) { return y_dots(d.group) + y_dots.bandwidth()/2; })
            
        // create svg element
        var dots = d3.select(containerId).selectAll("g");
        const t = 2000;
        dots.selectAll(".edges")
            .data(nested_data)
            .transition()
            .duration(t)
            .attr("d", function(d){ return addLine(d.values)});

        dots.selectAll(".dots")
            .data(data)
                .transition()
                .duration(t)
                .attr("cx", function (d) { return x_dots(d.percent); } )
                .attr("cy", function (d) { return y_dots(d.group) +  y_dots.bandwidth()/2; } )
                .style("stroke", function(d) {return d.year === 2012 ? "#808080": "#000000" })
                .style("stroke-width", function(d) {return d.year === 2012 ? 1.5: 1 })
                .style("fill", function(d) {return d.year === 2012 ? "#FFFFFF": color_dots(d.race) })
                .attr("class", "dots");
                        
            // update xaxis and title
            dots.selectAll('g.xaxis')
                .transition()
                .duration(t)
                .call(xAxis_dots.tickFormat(d3.format(d3.max(data, function(d) {return d.percent;})*1.1 < .05 ? ".1%": ".0%")));
      });
  }
  
  