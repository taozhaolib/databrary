'use strict'

module.controller('volume/slot', [
  '$scope', 'slot', 'edit', 'pageService', 'Store', 'Segment',
  ($scope, slot, editing, page, Store, Segment) ->
    page.display.title = slot.displayName
    $scope.flowOptions = Store.flowOptions
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.editing = editing
    $scope.mode = if editing then 'edit' else 'view'
    $scope.form = {}

    searchLocation = (url) ->
      url
        .search('asset', $scope.track?.asset?.id)
        .search('record', $scope.record?.id)

    if editing || slot.checkPermission(page.permission.EDIT)
      url = if editing then slot.route else slot.editRoute()
      page.display.toolbarLinks.push
        type: 'yellow'
        html: page.constants.message(if editing then 'slot.view' else 'slot.edit')
        url: url
        click: -> searchLocation(page.$location.url(url))


    updateRange = (segment) ->
      if isFinite(slot.segment.l)
        $scope.range.l = slot.segment.l
      else if isFinite(segment.l) && segment.l < $scope.range.l
        $scope.range.l = segment.l
      
      if isFinite(slot.segment.u)
        $scope.range.u = slot.segment.u
      else if isFinite(segment.u) && segment.u > $scope.range.u
        $scope.range.u = segment.u

    offsetPosition = (offset) ->
      (offset - $scope.range.base) / ($scope.range.u - $scope.range.l)

    positionOffset = (position) ->
      tl = $('#slot-timeline')
      p = (position - tl.offset().left) / tl.outerWidth()
      if p >= 0 && p <= 1
        $scope.range.l + p * ($scope.range.u - $scope.range.l)

    $scope.positionStyle = (p) ->
      styles = {}
      return styles unless p?
      if p instanceof page.models.Segment
        l = offsetPosition(p.l)
        r = offsetPosition(p.u)
      else
        l = offsetPosition(p)
      if l >= 0 && l <= 1
        styles.left = 100*l + '%'
      if r >= 0 && r <= 1
        styles.right = 100 - 100*r + '%'
      styles

    seekOffset = (o) ->
      if video && $scope.track?.asset?.segment.contains(o)
        video[0].currentTime = (o - $scope.track.asset.segment.base) / 1000
      $scope.position = o

    $scope.seekPosition = (pos) ->
      if o = positionOffset(pos)
        seekOffset(o)

    $scope.play = ->
      if video
        video[0].play()
      else
        $scope.playing = 1

    $scope.pause = ->
      if video
        video[0].pause()
      else
        $scope.playing = 0

    sortTracks = ->
      return unless $scope.tracks
      $scope.tracks.sort (a, b) ->
        if a.asset && b.asset
          isFinite(b.asset.segment.l) - isFinite(a.asset.segment.l) ||
            a.asset.segment.l - b.asset.segment.l ||
            a.asset.segment.u - b.asset.segment.u ||
            a.id - b.id
        else
          !a.asset - !b.asset || !a.file - !b.file

    targetAsset = page.$location.search().asset
    targetRecord = page.$location.search().record

    selectRange = (range) ->
      if isFinite(range.l)
        seekOffset(range.l)

    select = (track, record, consent) ->
      $scope.track.dirty = $scope.form.edit.$dirty if $scope.track && $scope.form.edit

      $scope.track = track
      $scope.record = record
      $scope.consent = consent
      searchLocation(page.$location)
      targetAsset = undefined
      targetRecord = undefined

      $scope.playing = 0
      delete $scope.replace
      if track
        selectRange(track.asset?.segment)
        if $scope.form.edit
          if $scope.track.dirty
            $scope.form.edit.$setDirty()
          else
            $scope.form.edit.$setPristine()
      if record
        selectRange(record.segment)
      if consent
        selectRange(consent.segment)

    removed = (track) ->
      return if track.asset || track.file
      select() if track == $scope.track
      $scope.tracks.remove(track)

    class Track extends Store
      constructor: (asset) ->
        super slot, asset

      setAsset: (asset) ->
        super asset
        return unless asset
        updateRange(asset.segment)
        @select() if `asset.id == targetAsset`

      select: (event) ->
        return select this unless $scope.track == this
        $scope.seekPosition event.clientX if event

      positionStyle: ->
        $scope.positionStyle @asset?.segment

      remove: ->
        r = super()
        return removed this unless r?.then
        r.then (done) =>
          removed this if done

      save: ->
        super().then (done) =>
          return unless done
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.track
          sortTracks()

      upload: (file) ->
        delete $scope.replace
        super(file).then (done) =>
          return unless done
          # add a new blank track
          $scope.tracks.push(new Track()) unless @asset

      replace: ->
        $scope.replace = !$scope.replace

    $scope.fileAdded = (file) ->
      return unless editing
      $scope.track?.upload(file)

    $scope.fileSuccess = Store.fileSuccess
    $scope.fileProgress = Store.fileProgress

    videoEvents =
      pause: ->
        $scope.playing = 0
      playing: ->
        $scope.playing = 1
      ratechange: ->
        $scope.playing = video[0].playbackRate
      timeupdate: ->
        if $scope.track?.asset
          $scope.position = $scope.track.asset.segment.base + 1000*video[0].currentTime
      ended: ->
        $scope.playing = 0
        # look for something else to play?

    for ev, fn of videoEvents
      videoEvents[ev] = $scope.$lift(fn)

    @deregisterVideo = (v) ->
      return unless video == v
      video = undefined
      v.off(videoEvents)

    @registerVideo = (v) ->
      this.deregisterVideo video if video
      video = v
      v.on(videoEvents)

    $scope.selectRecord = (record) -> select(undefined, record)

    fillRecords = ->
      records = slot.records
      for r in records when !r.record
        r.record = slot.volume.records[r.id]
      records.sort (a, b) ->
        a.record.category - b.record.category || a.id - b.id

      t = []
      overlaps = (r) -> s.overlaps(r.segment)
      for r in records
        updateRange s = r.segment = Segment.make(r.segment)
        for o, i in t
          break unless o[0].record.category != r.record.category || o.some(overlaps)
        t[i] = [] unless i of t
        t[i].push(r)
        $scope.selectRecord(r) if `r.id == targetRecord`
      $scope.records = t

    $scope.recordMetrics = (r) ->
      ident = page.constants.category[r.record.category]?.ident || [page.constants.metricName.ident.id]
      metrics = []
      for m of r.record.measures
        m = +m
        metrics.push(m) unless m in ident
      metrics.sort (a, b) -> a - b

    $scope.selectConsent = (consent) -> select(undefined, undefined, consent)

    fillConsents = ->
      if Array.isArray(consents = slot.consents)
        for c in consents
          c.segment = Segment.make(c.segment)
      else
        consents = [
          segment: Segment.full
          consent: consents || 0
        ]
      $scope.consents = consents

    $scope.consentClasses = (c) ->
      cn = page.constants.consent[c.consent]
      cls = ['consent', 'icon', cn, 'hint-consent-' + cn]
      cls.push('slot-consent-select') if $scope.consent == c
      cls

    $scope.range = new page.models.Segment(Infinity, -Infinity)
    # implicitly initialize from slot.segment
    updateRange(page.models.Segment.full)

    $scope.tracks = (new Track(asset) for asset in slot.assets)
    $scope.tracks.push(new Track()) if editing
    sortTracks()

    fillRecords()
    fillConsents()

    $scope.playing = 0
    $scope.position = undefined

    video = undefined

    return
])
