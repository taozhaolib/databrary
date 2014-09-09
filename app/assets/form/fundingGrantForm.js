'use strict';

module.directive('fundingGrantForm', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var form = $scope.fundingGrantForm;
      form.funding = angular.isDefined($attrs.funding);

      form.volume = page.$parse($attrs.volume)($scope) || undefined;
      form.data = page.$parse($attrs.funder)($scope) || {};
      form.awards = [
        {
          val: '',
        }
      ];

      var backup = $.extend(true, {}, form.data);

      if (form.data.awards) {
        form.awards = form.data.awards.map(function (award) {
          return {
            val: award,
          };
        });

        if (form.awards.length === 0) {
          form.awards.push({
            val: '',
          });
        }
      }

      //

      form.extend = function () {
        form.data.inherit = form.data === form.data.inherit ? 0 : Math.min(form.data, page.permission.CONTRIBUTE);
      };

      //

      form.save = function () {
        form.data.awards = form.awards.filter(function(award){
	  return award.val;
	}).map(function (award) {
          return award.val.trim();
        });

	form.volume.fundingSave(form.data.funder.id, form.data).then(function () {
          form.messages.add({
            body: page.constants.message('funding.save.success'),
            type: 'green',
            countdown: 3000,
          });

          delete form.data.new;
          backup = $.extend(true, {}, form.data);
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('funding.save.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.reset = function () {
        form.validator.clearServer();

        if (form.data.awards) {
          form.awards = form.data.awards.map(function (award) {
            return {
              val: award,
            };
          });
        }

        form.data = $.extend(true, {}, backup);

        if (!form.data.new) {
          form.$setPristine();
        } else {
          form.remove();
        }
      };

      //

      form.removeSuccessFn = undefined;

      form.remove = function () {
	form.volume.fundingDelete(form.data.funder.id, form.data).then(function () {
          if (angular.isFunction(form.removeSuccessFn)) {
            form.removeSuccessFn();
          }

          form.messages.add({
            body: page.constants.message('funding.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          delete form.data.new;
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('funding.remove.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      //

      form.addAward = function () {
        if (!form.awards) {
          form.awards = [];
        }

        form.awards.push({});
      };

      form.removeAward = function (award) {
        var i = form.awards.indexOf(award);

        if (i > -1) {
          form.awards.splice(i, 1);
        }
	if (form.awards.length === 0)
	{
	  form.remove();
	}

        form.$setDirty();
      };

      //

      $scope.$emit('fundingGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'fundingGrantForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
