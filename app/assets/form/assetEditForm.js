'use strict';

module.directive('assetEditForm', [
  'pageService', function (page) {
      var link = function ($scope){
	var form = $scope.assetEditForm;

	form.updateData = function() {
	  form.slotAsset = $scope.ctrl.current;
	  form.data = {
	    id:		    form.slotAsset.asset.id,
	    name:	    form.slotAsset.asset.name,
	    classification: form.slotAsset.asset.classification+''
	  };
	};
	form.updateData();
	$scope.ctrl.updateEditData = form.updateData; //can this be done better?
	
	form.saveAsset = function() {
	  form.slotAsset.save(form.data).then(function (res){
	      form.$setPristine();
	      $scope.ctrl.timeline.parseTracks();
	      //TODO - success message?
	    },
	    function (error){
	      //TODO - error message? 
	    }
	  );
	};

    };

    //

    return {
      restrict: 'E',
      templateUrl: 'assetEditForm.html',
      link: link
    };
  }
]);
