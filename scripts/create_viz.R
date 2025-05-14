library(htmltools)

# Create the embedding visualization component
create_embedding_viz <- function() {
  # Create the HTML
  viz_html <- HTML('
    <div id="embedding-visualization" style="width: 100%; height: 200px; margin: 20px 0; background-color: rgba(194, 197, 170, 0.2); border-radius: 8px;"></div>

    <!-- Add D3.js library -->
    <script src="https://d3js.org/d3.v7.min.js"></script>

    <script>
      // Set up the visualization
      const width = document.getElementById("embedding-visualization").clientWidth;
      const height = 200;
      
      // Create the SVG container
      const svg = d3.select("#embedding-visualization")
        .append("svg")
        .attr("width", width)
        .attr("height", height);
      
      // Generate random points
      const numPoints = 40;
      const points = Array.from({length: numPoints}, () => ({
        x: Math.random() * width,
        y: Math.random() * height,
        group: Math.random() > 0.5 ? 1 : 2
      }));
      
      // Define target positions for clusters
      const cluster1 = {x: width * 0.25, y: height * 0.4};
      const cluster2 = {x: width * 0.75, y: height * 0.6};
      
      // Add points to the SVG
      const circles = svg.selectAll("circle")
        .data(points)
        .enter()
        .append("circle")
        .attr("cx", d => d.x)
        .attr("cy", d => d.y)
        .attr("r", 3)
        .attr("fill", d => d.group === 1 ? "#656d4a" : "#a68a64")
        .style("opacity", 0.7);
      
      // Add cluster backgrounds
      const clusterBg1 = svg.append("circle")
        .attr("cx", cluster1.x)
        .attr("cy", cluster1.y)
        .attr("r", 50)
        .attr("fill", "#656d4a")
        .style("opacity", 0)
        .style("transition", "opacity 2s ease-in-out");
      
      const clusterBg2 = svg.append("circle")
        .attr("cx", cluster2.x)
        .attr("cy", cluster2.y)
        .attr("r", 50)
        .attr("fill", "#a68a64")
        .style("opacity", 0)
        .style("transition", "opacity 2s ease-in-out");
      
      // Add interaction to organize points into clusters
      let organized = false;
      
      document.getElementById("embedding-visualization").addEventListener("click", () => {
        if (!organized) {
          // Move points to clusters
          circles.transition()
            .duration(2000)
            .attr("cx", d => {
              const target = d.group === 1 ? cluster1 : cluster2;
              const angle = Math.random() * Math.PI * 2;
              const radius = Math.random() * 40;
              return target.x + Math.cos(angle) * radius;
            })
            .attr("cy", d => {
              const target = d.group === 1 ? cluster1 : cluster2;
              const angle = Math.random() * Math.PI * 2;
              const radius = Math.random() * 40;
              return target.y + Math.sin(angle) * radius;
            });
          
          // Show cluster backgrounds
          clusterBg1.transition().duration(2000).style("opacity", 0.2);
          clusterBg2.transition().duration(2000).style("opacity", 0.2);
          
          organized = true;
        } else {
          // Scatter points again
          circles.transition()
            .duration(2000)
            .attr("cx", () => Math.random() * width)
            .attr("cy", () => Math.random() * height);
          
          // Hide cluster backgrounds
          clusterBg1.transition().duration(2000).style("opacity", 0);
          clusterBg2.transition().duration(2000).style("opacity", 0);
          
          organized = false;
        }
      });
    </script>
  ')
  
  return(viz_html)
}

# Create a test HTML file
test_html <- HTML(paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Embedding Visualization Test</title>
    <style>
        body {
            font-family: "Open Sans", sans-serif;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
        }
        h1 {
            color: #333d29;
        }
        p {
            margin-bottom: 20px;
        }
    </style>
</head>
<body>
    <h1>Embedding Visualization Test</h1>
    <p>Click on the visualization to toggle between scattered and clustered states.</p>
    
    ', as.character(create_embedding_viz()), '
</body>
</html>
'))

# Write to a file
writeLines(as.character(test_html), "test-visualization.html")

# Open the file in a browser
if(interactive()) {
  browseURL("test-visualization.html")
}