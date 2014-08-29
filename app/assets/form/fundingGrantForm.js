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

      form.saveFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      form.save = function () {
        form.data.awards = form.awards.map(function (award) {
          return award.val.trim();
        }).filter(function (grant) {
          return grant !== '';
        });

        if (angular.isFunction(form.saveFn)) {
          form.saveFn(form);
        }

	form.volume.fundingSave(form.data.funder.id, form.data).then(function () {
          if (angular.isFunction(form.successFn)) {
            form.successFn(form, arguments);
          }

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

          if (angular.isFunction(form.errorFn)) {
            form.errorFn(form, arguments);
          }

          page.display.scrollTo(form.$element);
        });
      };

      form.resetFn = undefined;

      form.reset = function () {
        if (angular.isFunction(form.resetFn)) {
          form.resetFn(form);
        }

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

      form.removeFn = undefined;
      form.removeSuccessFn = undefined;
      form.removeErrorFn = undefined;

      form.remove = function () {
        if (angular.isFunction(form.removeFn)) {
          form.removeFn(form);
        }

	form.volume.fundingDelete(form.data.funder.id, form.data).then(function () {
          if (angular.isFunction(form.removeSuccessFn)) {
            form.removeSuccessFn(form, arguments, form.access);
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

          if (angular.isFunction(form.removeErrorFn)) {
            form.removeErrorFn(form, arguments, form.access);
          }

          page.display.scrollTo(form.$element);
        });
      };

      //

      form.addAward = function () {
        if (!form.awards) {
          form.awards = [];
        }

        form.awards.push({});
        form.$setDirty();
      };

      form.removeAward = function (award) {
        var i = form.awards.indexOf(award);

        if (i > -1) {
          form.awards.splice(i, 1);
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
