module.factory('modelService', [
	'$rootScope',
	'Asset',
	'Comment',
	'Party',
	'PartyAuthorize',
	'Record',
	'Scraper',
	'Slot',
	'SlotAsset',
	'Tag',
	'Volume',
	'VolumeAccess',
	function ($rootScope, Asset, Comment, Party, PartyAuthorize, Record, Scraper, Slot, SlotAsset, Tag, Volume, VolumeAccess) {
		var models = {
			Asset: Asset,
			Comment: Comment,
			Party: Party,
			PartyAuthorize: PartyAuthorize,
			Record: Record,
			Scraper: Scraper,
			Slot: Slot,
			SlotAsset: SlotAsset,
			Tag: Tag,
			Volume: Volume,
			VolumeAccess: VolumeAccess
		};

		return models;
	}
]);
