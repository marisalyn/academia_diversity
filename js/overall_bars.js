var selected_uni = "none" // initial 

var margin_bars = {
  top: 50,
  right: 80,
  bottom: 80,
  left: 180
};

// calculate dimensions
var width_bars = 1000;
var height_bars = 420;
var innerWidth_bars = width_bars - margin_bars.left - margin_bars.right;
var innerHeight_bars = height_bars - margin_bars.top - margin_bars.bottom;

// create svg element
var bars = d3.select("#overall_bars")
    .append("svg")
    .attr("height", height_bars)
    .attr("width", width_bars)
    .append("g")
    .attr("transform", "translate(" + margin_bars.left + "," + margin_bars.top + ")");

//define div for tooltip 
var tooltip_bars = d3.select("body")
    .append("div")
    .attr("class", "tooltip")
    .style("opacity", 0); 

function handleError(error, msg) {
  if (error) {
      console.error(msg);
  }
}

// set x, y and colors
var x_bars = d3.scaleLinear()
    .rangeRound([0, innerWidth_bars])
    .domain([0,1]);

var y_bars = d3.scaleBand()
    .rangeRound([0, innerHeight_bars])
    .paddingInner(0.15);

var color_bars = d3.scaleOrdinal()
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
          
var usBarWidth = 30;

// get overall data
function buildBars() {
  d3.csv('data/processed/overall_race.csv',  function(error, data) { 
    //console.log("initial bar data", data)
    data = data.filter(d => +d.year == 2018)
    names = d3.nest()
              .key(function(d) { return d.group; })
              .entries(data)
              .map(function(d) { return d.key; });

    races = d3.nest()
              .key(function(d) { return d.race; })
              .entries(data.filter(d => d.year == 2018))
              .map(function(d) { return d.key; });
    
    var grouped = names.map(function(d) { 
      var item = { name:  d };
      races.forEach(function(e) {
        var itemForGroup = data.filter(function(f) {
          return f.group === d && f.race === e;
        });
        if (itemForGroup.length) {
          item[e] = + itemForGroup[0].percent;
        } else {
          item[e] = 0;
        }
      })
      return item;
    });
      
    var stack = d3.stack().keys(Object.keys(grouped[0]).filter(function(k) { return k !== "name"; }));
    var stacked_data = stack(grouped);
    //console.log("stacked data", stacked_data)

    y_bars.domain(names)
    color_bars.domain(races)
    
    bars
       .selectAll('.bar')
       .data(stacked_data)
       .enter()
       .append("g")
       .style('fill', (d) => (color_bars(d.key)))
       .selectAll("rect")
       .data(function(d) {return d;})
       .enter().append("rect")
          .attr('y', d => y_bars(d.data.name))
          .attr('x', d => x_bars(d[0])) 
          .attr('width', d => x_bars(d[1]) - x_bars(d[0]))
          .attr("height", function(d,i) {
           if(i == 0) { return usBarWidth;}
           return y_bars.bandwidth();
          })
          .on("mouseover", function() { tooltip_bars.style("display", null); })
          .on("mouseout", function() { tooltip_bars.style("display", "none"); })
          .on("mousemove", function(d, i) {
            tooltip_bars.html(
              d3.select(this.parentNode).datum().key + 
              "<br>" + 
              d3.format(".2%")(d[1] - d[0])
              ) 
                .style("left", d3.event.pageX  + 20 + "px")
                .style("top", d3.event.pageY + 20+ "px")
                .style("opacity", 1);
          });

        // add axes
        bars.append('g')
            .attr('class', 'yaxis')
            .call(d3.axisLeft(y_bars).tickSizeOuter(0))
            .call(g => g.select(".domain").remove())
            .call(g => g.selectAll('.tick > line').remove());

        bars.selectAll('.yaxis text')
            .attr('transform', (d, i) => `translate(0, ${ i == 0 ? -usBarWidth/2 + 3 : 0})`);

        bars.append('g')
            .attr('class', 'xaxis')
            .call(d3.axisBottom(x_bars).tickSizeOuter(0).tickFormat(d3.format(".0%")))
            .attr('transform', `translate(0, ${ innerHeight_bars + 10})`);

        bars.append('g')
           .attr('class', 'xaxis')
           .call(d3.axisBottom(x_bars).tickSizeOuter(0).tickFormat(d3.format(".0%")))
           .attr('transform', `translate(0, ${ usBarWidth + 10})`);

        // add title
        bars.append('text')
           .attr("class", "title")
           .attr("x", innerWidth_bars / 2)
           .attr("y", -25)
           .attr("text-anchor", 'middle')
           .text("Racial and ethnic diversity across academia (2018)");
});
}
    
buildBars();

function updateBars(selected_uni, selected_uni_name) {
  d3.csv('data/processed/all_race_2018.csv',  function(error, data) { 
    // d3 filter is SLOW --> create smaller DFs to filter in R
    data = data.filter(d => d.uni_id == selected_uni || d.group == "U.S. population")
    names = d3.nest()
        .key(function(d) { return d.group; })
        .entries(data)
        .map(function(d) { return d.key; });

    races = d3.nest()
        .key(function(d) { return d.race; })
        .entries(data)
        .map(function(d) { return d.key; });

    var grouped = names.map(function(d) { 
    var item = { name:  d };
    races.forEach(function(e) {
        var itemForGroup = data.filter(function(f) {
        return f.group === d && f.race === e;
        });
        if (itemForGroup.length) {
            item[e] = + itemForGroup[0].percent;
        } else {
            item[e] = 0;
        }
    })
    return item;
    });

    var stack = d3.stack().keys(Object.keys(grouped[0]).filter(function(k) { return k !== "name"; }));
    var stacked_data = stack(grouped);
    console.log("stacked data 2", stacked_data)

    y_bars.domain(names)
    color_bars.domain(races)

    bars.selectAll(".bar")
        .exit().remove();

    bars
        .selectAll('.bar')
        .data(stacked_data)
        .enter()
        .append("g")
        .style('fill', (d) => (color_bars(d.key)))
        .selectAll("rect")
        .data(function(d) {return d;})
        .enter().append("rect")
           .attr('y', d => y_bars(d.data.name))
           .attr('x', d => x_bars(d[0])) 
           .attr('width', d => x_bars(d[1]) - x_bars(d[0]))
           .attr("height", function(d,i) {
            if(i == 0) { return usBarWidth;}
            return y_bars.bandwidth();
           })
           .on("mouseover", function() { tooltip_bars.style("display", null); })
           .on("mouseout", function() { tooltip_bars.style("display", "none"); })
           .on("mousemove", function(d, i) {
            tooltip_bars.html(d3.select(this.parentNode).datum().key + 
                          "<br>" + 
                          d3.format(".2%")(d[1] - d[0])) 
                    .style("left", d3.event.pageX  + 20 + "px")
                    .style("top", d3.event.pageY + 20+ "px")
                    .style("opacity", 1);
           });

    bars.selectAll("text").remove();

    // add axes
    bars.append('g')
      .attr('class', 'yaxis')
      .call(d3.axisLeft(y_bars).tickSizeOuter(0))
      .call(g => g.select(".domain").remove())
      .call(g => g.selectAll('.tick > line').remove())

    bars.selectAll('.yaxis text')
       .attr('transform', (d, i) => `translate(0, ${ i == 0 ? -usBarWidth/2 + 3 : 0})`)
    bars.append('g')
       .attr('class', 'xaxis')
       .call(d3.axisBottom(x_bars).tickSizeOuter(0).tickFormat(d3.format(".0%")))
       .attr('transform', `translate(0, ${ innerHeight_bars + 10})`)
    bars.append('g')
       .attr('class', 'xaxis')
       .call(d3.axisBottom(x_bars).tickSizeOuter(0).tickFormat(d3.format(".0%")))
       .attr('transform', `translate(0, ${ usBarWidth + 10})`);

    bars.append('text')
        .attr("class", "title")
        .attr("x", innerWidth_bars / 2)
        .attr("y", -25)
        .attr("text-anchor", 'middle')
        .text("Racial and ethnic diversity at " + selected_uni_name + " (2018)");
});
}

// d3.select("#map").on("click", function(d) {
//   selected_uni = clicked;
//   selected_uni_name = clicked_name;
//   updateBars(selected_uni, selected_uni_name)
// });


