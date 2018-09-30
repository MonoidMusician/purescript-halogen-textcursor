exports.computedStyle = function(element) {
  return function() {
    return getComputedStyle(element, null).cssText;
  };
};
