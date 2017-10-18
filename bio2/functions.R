
plotshot = function(){
  webshot::webshot(url='http://127.0.0.1:5949/', delay=10, selector='#map') # #map
  
  url = 'http://127.0.0.1:5949/?_inputs_&btn_save=0&countries=null&map_bounds=%7B%22north%22%3A12.2541277376574%2C%22east%22%3A3.779296875%2C%22south%22%3A-11.7383023714368%2C%22west%22%3A-46.5380859375%7D&map_center=%7B%22lng%22%3A-21.37939453125%2C%22lat%22%3A0.26367094433665%7D&map_groups=%5B%22Gray%20Land%22%2C%22Seascape%22%2C%22EEZ%22%5D&map_shape_mouseout=%7B%22id%22%3A%22Brazil%22%2C%22.nonce%22%3A0.911747647202316%2C%22group%22%3A%22EEZ%22%2C%22lat%22%3A-5.26600788280548%2C%22lng%22%3A-36.3427734375%7D&map_shape_mouseover=%7B%22id%22%3A%22Brazil%22%2C%22.nonce%22%3A0.871951147846154%2C%22group%22%3A%22EEZ%22%2C%22lat%22%3A-3.11857621678199%2C%22lng%22%3A-36.2548828125%7D&map_zoom=5&sel_eez=%22%22&sel_grd=%22%2FVolumes%2FBest%20HD%2Fmbon_data_big%2Fsatellite%2Fseascapes%2Fgl%2FGLOBE14_I90VAR3_9k_leaflet.grd%22&sel_lyr=%22ymd_2002.07.15%22&sidebarCollapsed=false&sidebarItemExpanded=null&tabset_viz=%22%3Ci%20class%3D%5C%22fa%20fa-globe%5C%22%3E%3C%5C%2Fi%3E%5CnMap%22&ts_streamgraph-select=null'
  
  webshot::webshot(url, delay=15, selector='#map') # #map
}