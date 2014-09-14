'use strict';

module.directive('volumeEditFundingForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditFundingForm;

      form.data = volume.funding.slice();

      var subforms = [];

      function checkDirty() {
	if (!subforms.some(function (subform) {
	      return subform.$dirty;
	    }))
	  form.$setPristine();
      }

      form.saveAll = function () {
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
      };

      $scope.$on('fundingGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (funding) {
          form.data.splice(form.data.indexOf(funding), 1);
          subforms.splice(subforms.indexOf(grantForm), 1);
	  checkDirty();
        };
      });

      $scope.$on('fundingSearchForm-init', function (event, searchForm) {
        searchForm.selectFn = function (found) {
	  if (form.data.some(function (funding) {
		return funding.funder.id === found.id;
	      })) {
            searchForm.messages.add({
              type: 'yellow',
              countdown: 3000,
              body: page.constants.message('funding.search.repeat', found.name),
            });
	    return;
	  }

	  form.data.push({
	    funder: found,
	    awards: [],
	    new: true,
	  });

	  //warning: next line is template dependent! if classnames or structure change this may no longer work
	  page.$timeout(function() {
	    var newEl = $('form fieldset.permission-auth').last();
	    page.display.scrollTo(newEl);
	  });
        };
      });

      var $float = $('.vef-float');
      var $floater = $('.vef-float-floater');
      form.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll(form.scrollFn);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volumeEditFundingForm.html',
      link: link
    };
  }
]);
