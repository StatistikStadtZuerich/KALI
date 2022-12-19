"use strict";

/*
This file contains an example from SSZVIS website modified to work with Shiny.
https://statistikstadtzuerich.github.io/sszvis/#/bar-chart-vertical

Global variables exposed by Shiny (dependencies): d3, sszvis
*/

// Configuration
// -----------------------------------------------
// Matches the one set in Shiny.
var CHART_CONTAINER_ID = "#sszvis-chart";
/*var MAX_WIDTH = 3000;*/
var queryProps = sszvis
          .responsiveProps()
          .prop("yLabel", {
            _: "Anzahl Stimmen",
          })
          .prop("ticks", {
            _: 5,
          });

function parseRow(d) {
  return {
    category: d["StimmeVeraeListe"],
    // No need to parse numbers, as the data sent from Shiny is already ok.
    xValue: d["Value"],
  };
}
var cAcc = sszvis.prop("category");
var xAcc = sszvis.prop("xValue");

// Application state
// -----------------------------------------------

var state = {
  data: [],
  categories: [],
  selected: [],
};

// State transitions
// -----------------------------------------------
var actions = {
  prepareState: function prepareState(data) {
    state.data = data;
    state.categories = sszvis.set(state.data, cAcc);
    render(state);
  },

  showTooltip: function showTooltip(_, category) {
    state.selected = state.data.filter(function (d) {
      return cAcc(d) === category;
    });
    render(state);
  },

  hideTooltip: function hideTooltip() {
    state.selected = [];
    render(state);
  },

  resize: function resize() {
    render(state);
  },
};

// Render
// -----------------------------------------------
function render(state) {
  var props = queryProps(sszvis.measureDimensions(CHART_CONTAINER_ID));
  var chartDimensions = sszvis.dimensionsHorizontalBarChart(
    state.categories.length
  );
  var bounds = sszvis.bounds(
    {
      height: 30 + chartDimensions.totalHeight + 40,
      top: 30,
      bottom: 40,
    },

    CHART_CONTAINER_ID
  );

  var chartWidth = Math.min(bounds.innerWidth, 2000);

  // Scales

  var widthScale = d3
    .scaleLinear()
    .range([0, chartWidth])
    .domain([0, d3.max(state.data, xAcc)]);

  var yScale = d3
    .scaleBand()
    .padding(chartDimensions.padRatio)
    .paddingOuter(chartDimensions.outerRatio)
    .rangeRound([0, chartDimensions.totalHeight])
    .domain(state.categories);

  // Layers

  var chartLayer = sszvis
    .createSvgLayer(CHART_CONTAINER_ID, bounds)
    .datum(state.data);

  var controlLayer = sszvis.createHtmlLayer(CHART_CONTAINER_ID, bounds);

  var tooltipLayer = sszvis
    .createHtmlLayer(CHART_CONTAINER_ID, bounds)
    .datum(state.selected);

  // Components

  var barGen = sszvis
    .bar()
    .x(0)
    .y(sszvis.compose(yScale, cAcc))
    .width(sszvis.compose(widthScale, xAcc))
    .height(chartDimensions.barHeight)
    .centerTooltip(true)
    .fill(sszvis.scaleQual12());

  var xAxis = sszvis
    .axisX()
    .scale(widthScale)
    .orient("bottom")
    .alignOuterLabels(true)
    .title(props.yLabel);

  if (props.ticks) {
    xAxis.ticks(props.ticks);
  }

  var yAxis = sszvis.axisY.ordinal().scale(yScale).orient("right");
  
  var tooltipHeader = sszvis
    .modularTextHTML()
    .bold(function (d) {
      var xValue = xAcc(d);
      return isNaN(xValue) ? "keine" : sszvis.formatNumber(xValue);
    })
    .plain("Stimmen");


  var tooltip = sszvis
    .tooltip()
    .renderInto(tooltipLayer)
    .orientation(sszvis.fitTooltip("bottom", bounds))
    .header(tooltipHeader)
    .visible(isSelected);

  // Rendering

  chartLayer.attr(
    "transform",
    sszvis.translateString(
      bounds.innerWidth / 2 - chartWidth / 2,
      bounds.padding.top
    )
  );

  var bars = chartLayer.selectGroup("bars").call(barGen);

  chartLayer
    .selectGroup("xAxis")
    .attr(
      "transform",
      sszvis.translateString(0, chartDimensions.totalHeight)
    )
    .call(xAxis);

  chartLayer
    .selectGroup("yAxis")
    .attr(
      "transform",
      sszvis.translateString(0, chartDimensions.axisOffset)
    )
    .call(yAxis);

  bars.selectAll("[data-tooltip-anchor]").call(tooltip);

  // As the chart is reset on dataset change, remove xAxis when this happens,
  // to prevent a single line appearing at the bottom of the chart.
  if (state.data.length > 0) {
    bars
      .selectGroup("cAxis")
      .attr("transform", sszvis.translateString(0, bounds.innerHeight))
      .call(xAxis);
  } else {
    bars.selectGroup("xAxis").remove();
  }

  chartLayer.selectGroup("yAxis").call(yAxis);

  // Interaction

  // Use the move behavior to provide tooltips in the absence of a bar, i.e.
  // when we have missing data.
  var interactionLayer = sszvis
    .move()
    .xScale(widthScale)
    .yScale(yScale)
    .cancelScrolling(isWithinBarContour)
    .fireOnPanOnly(true)
    .on("move", actions.showTooltip)
    .on("end", actions.hideTooltip);

  // Raise the group to correctly interact with mouse events.
  chartLayer.selectGroup("interaction").call(interactionLayer);

  sszvis.viewport.on("resize", actions.resize);
}


  function isWithinBarContour(xValue, category) {
    var barDatum = sszvis.find(function (d) {
      return cAcc(d) === category;
    }, state.data);
    return sszvis.util.testBarThreshold(xValue, barDatum, xAcc, 1000);
  }

  function isSelected(d) {
    return state.selected.indexOf(d) >= 0;
  }

/*
Shiny -> JS

We are listening for an "update_data" event sent from the server-side of the
Shiny app to run the following logic with a new data attached to the event (data).
It already comes in a form of parsed JSON object (array of observations), so there
is no need to parse the CSV / JSON anymore, as was the case in original example -
just a logic to extract needed properties is present (parseRow).
*/
Shiny.addCustomMessageHandler("update_data", function (data) {
  try {
    var parsedRows = data.map(parseRow);
    actions.prepareState(parsedRows);
  } catch (e) {
    throw e;
  }
});

