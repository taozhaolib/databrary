'use strict';

module.directive('ngForm', [
  'pageService', '$animate',
  function (page, $animate) {
    var pre = function ($scope, $element, $attrs) {
      var name = $attrs.name || $attrs.ngForm;
      var form = name && $scope.$eval(name);
      if (!form)
        return;

      form.messages = page.messages;
      form.$element = $element;

      var controls = [];

      function checkDirty() {
	if (controls.every(function (control) {
	    return control.$pristine;
	  })) {
	  /* effectively call form.$setPristine, without the controls. */
	  $animate.removeClass($element, 'ng-dirty');
	  $animate.addClass($element, 'ng-pristine');
	  form.$dirty = false;
	  form.$pristine = true;
	}
      }

      var $addControl = form.$addControl;
      var $removeControl = form.$removeControl;

      /* this is unfortunate, just because we can't access the existing controls list. */
      form.$addControl = function (control) {
	if ('$pristine' in control)
	  controls.push(control);
	return $addControl(control);
      };

      form.$removeControl = function (control) {
	controls.remove(control);
	checkDirty();
	return $removeControl(control);
      };

      form.subformControl = {
	$setPristine: checkDirty
      };

      /* it'd be nicer to handle this in $addControl, but it happens too early */
      var parentForm = $element.parent().controller('form');
      if (parentForm && parentForm.subformControl)
	form.$addControl(parentForm.subformControl);

      var unclaimed = {};
      form.validators = {};
      form.validator = {
	server: function (res, replace) {
	  if ($.isEmptyObject(res)) {
	    res.data = {};
	  } else if (!angular.isObject(res.data)) {
	    form.messages.addError({
	      body: page.constants.message('error.generic'),
	      report: res,
	    });
	    return;
	  }

	  var name;
	  for (name in form.validators) {
	    form.validators[name].server(res.data[name] || {}, replace);
	  }

	  for (name in res.data) {
	    if (form.validators[name]) {
	      form.validators[name].server(res.data[name], replace);
	    } else if (form.messages) {
	      form.messages.add({
		type: 'red',
		closeable: true,
		body: Array.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
	      });
	    }
	  }
	},

	clearServer: function () {
	  angular.forEach(form.validators, function (validator) {
	    validator.server({}, true);
	  });
	},

	client: function (data, replace) {
	  for (var name in data) {
	    if (!data.hasOwnProperty(name)) {
	      continue;
	    } else if (form.validators[name]) {
	      form.validators[name].client(data[name], replace);
	    } else {
	      unclaimed[name] = data[name];
	    }
	  }
	},

	add: function (name, validator) {
	  form.validators[name] = validator;

	  if (unclaimed[name]) {
	    validator.client(unclaimed[name], true);
	    delete unclaimed[name];
	  }
	},
      };

      form.resetAll = function (force, check) {
	if (!(force || form.$pristine || confirm(page.constants.message('navigation.confirmation'))))
	  return false;
	if (check)
	  return true;
	var x = window.pageXOffset,
	    y = window.pageYOffset;
	page.$route.reload();
	page.$timeout(function () {
	  window.scrollTo(x, y);
	});
	return true;
      };
    };

    return {
      restrict: 'EA',
      link: {
	pre: pre
      }
    };
  }
]);
