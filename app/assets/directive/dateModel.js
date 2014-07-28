'use strict';

module.directive('dateModel', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs, ngModel) {
      if (!ngModel) {
	return;
      }

      var supportsDate = document.createElement('input');
      supportsDate.setAttribute('type', 'date');
      supportsDate = supportsDate.type === 'date';

      angular.forEach(['dateMin', 'dateMax'], function (period) {
	$attrs[period] = $attrs[period] || '';

	switch (true) {
	  case $attrs[period] === 'now':
	    ngModel[period] = new Date().getTime();
	    break;

	  case $attrs[period].indexOf('year') > 0 || $attrs[period].indexOf('years') > 0:
	    ngModel[period] = new Date().setYear(new Date().getFullYear() + parseInt($attrs[period]));
	    break;
	}
      });

      ngModel.$parsers.push(function (value) {
	var changed;

	if (!value.match(/^\d{4}-\d{1,2}-\d{1,2}$/)) {
	  return;
	}

	if (supportsDate) {
	  value = value.split('-');
	  value = new Date(value[1] + '-' + value[2] + '-' + value[0]);
	} else {
	  value = new Date(value);
	}

	value = value.getTime();

	if (isNaN(value) || ngModel.dateMax && value > ngModel.dateMax) {
	  changed = true;
	  value = ngModel.dateMax;
	}

	if (ngModel.dateMin && value < ngModel.dateMin) {
	  changed = true;
	  value = ngModel.dateMin;
	}

	$element.off('blur.dateModel');

	if (changed) {
	  ngModel.$setValidity('dateModel', false);

	  $element.on('blur.dateModel', function () {
	    $scope.$apply(function () {
	      ngModel.$setValidity('dateModel', true);
	      ngModel.$viewValue = page.$filter('date')(value, 'yyyy-MM-dd');
	      $element[0].value = page.$filter('date')(value, 'yyyy-MM-dd');
	    });

	    $element.off('blur.dateModel');
	  });
	} else {
	  ngModel.$setValidity('dateModel', true);
	}

	return value;
      });

      ngModel.$formatters.push(function (value) {
	return page.$filter('date')(value, 'yyyy-MM-dd');
      });
    };

    return {
      restrict: 'A',
      require: 'ngModel',
      link: link,
    };
  }
]);
