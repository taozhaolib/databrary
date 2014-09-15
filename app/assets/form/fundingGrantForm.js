'use strict';

module.directive('fundingGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var funding = $scope.funding;
      var form = $scope.fundingGrantForm;

      form.data = {
	awards: funding.awards && funding.awards.length ? funding.awards.slice(0) : ['']
      };

      if (funding.new)
	form.$setDirty();

      form.save = function () {
        form.data.awards = form.data.awards
	  .map(function (grant) {
	    return grant.trim();
	  }).filter(function (grant) {
	    return grant !== '';
	  });

	volume.fundingSave(funding.funder.id, form.data).then(function () {
          form.messages.add({
            body: page.constants.message('funding.save.success'),
            type: 'green',
            countdown: 3000,
          });

          delete funding.new;
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('funding.save.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.remove = function () {
	volume.fundingDelete(funding.funder.id).then(function () {
          form.messages.add({
            body: page.constants.message('funding.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.$setPristine();
	  form.removeSuccessFn(funding);
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
	form.data.awards.push('');
      };

      form.addAwardEnabled = function() {
	return form.data.awards.every(function (grant) {
	  return grant.trim() !== '';
	});
      };

      form.removeAward = function (i) {
	form.data.awards.splice(i, 1);
        form.$setDirty();
	if (!form.data.awards.length)
	  form.remove();
      };

      //

      $scope.$emit('fundingGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'fundingGrantForm.html',
      link: link
    };
  }
]);
