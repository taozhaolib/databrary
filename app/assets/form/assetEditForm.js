'use strict';

module.directive('assetEditForm', [
  'pageService', function (page) {
      var link = function ($scope){
	var form = $scope.assetEditForm;

	form.updateData = function() {
	  form.slotAsset = $scope.ctrl.current;
	  form.data = {
	    name:	    form.slotAsset.asset.name,
	    id:		    form.slotAsset.asset.id,
	    classification: form.slotAsset.asset.classification+''
	  };
	};
	form.updateData();
	$scope.ctrl.updateEditData = form.updateData; //can this be done better?
	
	form.saveAsset = function() {
	  form.slotAsset.save(form.data).then(function (res){
	      form.$setPristine();
	      form.messages.add({
		type: 'green',
		countdown: 3000,
		body: page.constants.message('asset.update.success', res.name),
	      });
	    },
	    function (error){
	      form.messages.addError({
		type: 'red',
		body: page.constants.message('asset.update.error', form.data.name),
		report: error,
	    });
	  }
	  );
	};

	form.removeAsset = function(conf){
	  if(conf && !confirm(page.constants.message('asset.remove.confirm'))) return;
	  form.slotAsset.remove().then(function() {
	    form.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('asset.remove.success', form.data.name || page.constants.message('file')),
	    });
	    $scope.ctrl.timeline.tracks.splice($scope.ctrl.timeline.tracks.indexOf(form.slotAsset, 1));
	    $scope.ctrl.current = undefined;
	  }, function (res) {
	    form.messages.addError({
	      type: 'red',
	      body: page.constants.message('asset.remove.error', form.data.name || page.constants.message('file')),
	      report: res,
	    });
	  });
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
