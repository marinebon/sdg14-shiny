var tile_url = template(this._url, extend(data, this.options));
if (tile_url.match('earthdata') != null){
    console.log(tile_url);
    debugger;
}
return tile_url;

mac
// getTileUrl: function (coords) {
//
//   var tileBounds = this._tileCoordsToNwSe(coords),
//       crs = this._crs,
//       bounds = toBounds(crs.project(tileBounds[0]), crs.project(tileBounds[1])),
//       min = bounds.min,
//       max = bounds.max,
//       bbox = (this._wmsVersion >= 1.3 && this._crs === EPSG4326 ?
//       [min.y, min.x, max.y, max.x] :
//       [min.x, min.y, max.x, max.y]).join(','),
//   url = L.TileLayer.prototype.getTileUrl.call(this, coords);
  var url_plus = url +
    getParamString(this.wmsParams, url, this.options.uppercase) +
    (this.options.uppercase ? '&BBOX=' : '&bbox=') + bbox;
  if (url_plus.match('earthdata') != null){
        console.log(url_plus);
        debugger;
  }
  return url_plus
