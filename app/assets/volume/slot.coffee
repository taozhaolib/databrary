'use strict'

app.controller('volume/slot', [
  '$scope', '$location', '$sce', '$q', '$timeout', 'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', 'storageService', 'Offset', 'Segment', 'Store', 'slot', 'edit',
  ($scope, $location, $sce, $q, $timeout, constants, display, messages, tooltips, styles, storage, Offset, Segment, Store, slot, editing) ->
    display.title = slot.displayName
    $scope.flowOptions = Store.flowOptions
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.editing = editing # $scope.editing (but not editing) is also a modal (toolbar) indicator
    $scope.mode = if editing then 'edit' else 'view'
    target = $location.search()
    $scope.form = {} # all ng-forms are put here
    fullRange = new Segment(0, 0) # total computed (asset + record) extent of this container (for zoom out full)
    ruler = $scope.ruler =
      range: if 'range' of target then new Segment(target.range) else fullRange # currently displayed zoom range
      selection: new Segment(if 'select' of target then target.select else null) # current temporal selection
      position: Offset.parse(target.pos) # current position, also asset seek point (should be within selection)
      zoomed: 'range' of target # are we currently zoomed in?

    $flow = $scope.$flow # not really in this scope
    video = undefined
    blank = undefined

    searchLocation = (url) ->
      url
        .search('asset', undefined)
        .search('record', undefined)
        .search($scope.current?.type || '', $scope.current?.id)
        .search('select', if !ruler.selection.empty then ruler.selection.format())
        .search('range', if ruler.zoomed then ruler.range.format())

    $scope.toggleEdit = () ->
      searchLocation($location.url(if editing then slot.route() else slot.editRoute()))

    byId = (a, b) -> a.id - b.id
    byPosition = (a, b) -> a.segment.l - b.segment.l
    finite = (args...) -> args.find(isFinite)

    updateRange = () ->
      l = Infinity
      u = -Infinity
      for t in $scope.tracks.concat(records, $scope.consents)
        l = t.segment.l if isFinite(t.segment?.l) && t.segment.l < l
        u = t.segment.u if isFinite(t.segment?.u) && t.segment.u > u
      fullRange.l = finite(slot.segment.l, l, 0)
      fullRange.u = finite(slot.segment.u, u, 0)
      return

    offsetPosition = (offset) ->
      return offset unless isFinite offset
      (offset - ruler.range.base) / (ruler.range.u - ruler.range.l)

    tl = document.getElementById('slot-timeline')
    positionOffset = (position) ->
      tlr = tl.getBoundingClientRect()
      p = (position - tlr.left) / tlr.width
      if p >= 0 && p <= 1
        ruler.range.l + p * (ruler.range.u - ruler.range.l)
      else if p < 0
        -Infinity
      else if p > 1
        Infinity

    $scope.positionStyle = (p) ->
      style = {}
      return style unless p?
      if p instanceof Segment
        l = offsetPosition(p.l)
        if l < 0
          style.left = '0px'
          style['border-left'] = '0px'
          style['border-top-left-radius'] = '0px'
          style['border-bottom-left-radius'] = '0px'
        else if l <= 1
          style.left = 100*l + '%'
        r = offsetPosition(p.u)
        if r > 1
          style.right = '0px'
          style['border-right'] = '0px'
          style['border-top-right-radius'] = '0px'
          style['border-bottom-right-radius'] = '0px'
        else if r >= 0
          style.right = 100 - 100*r + '%'
      else
        p = offsetPosition(p)
        if p >= 0 && p <= 1
          style.left = 100*p + '%'
      style

    $scope.updatePosition = () ->
      if video && $scope.asset?.segment.contains(ruler.position) && isFinite($scope.asset.segment.l)
        video[0].currentTime = (ruler.position - (if $scope.editing == 'position' then $scope.current.segment.l else $scope.asset.segment.l)) / 1000
      return

    seekOffset = (o) ->
      ruler.position = o
      $scope.updatePosition()
      return

    seekPosition = (pos) ->
      if isFinite(o = positionOffset(pos))
        seekOffset(o)
        unless ruler.selection.contains(o) || ruler.selection.u == o # "loose" contains
          ruler.selection = new Segment(null)
          finalizeSelection()
      return

    $scope.play = ->
      if video
        video[0].play()
      else
        $scope.playing = 1
      return

    $scope.pause = ->
      if video
        video[0].pause()
      else
        $scope.playing = 0
      return

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
      return

    stayDirty = (global) ->
      if global || editing && $scope.current && $scope.form.edit && ($scope.current.asset || $scope.current.record) && ($scope.current.dirty = $scope.form.edit.$dirty)
        not confirm(constants.message('navigation.confirmation'))

    select = (c) ->
      return false if stayDirty()

      $scope.current = c
      $scope.asset = c?.asset if !c || c.type == 'asset'
      searchLocation($location.replace())
      delete target.asset
      delete target.record
      if $scope.form.edit
        if c?.dirty
          $scope.form.edit.$setDirty()
        else
          $scope.form.edit.$setPristine()

      blank.fillData() if blank && c == blank
      $scope.playing = 0
      finalizeSelection()
      updatePlayerHeight()
      true

    $scope.selectAll = (event, c) ->
      return false if $scope.editing == 'position'
      ruler.selection = range = new Segment(c.segment)
      finalizeSelection()
      if range && isFinite(range.l) && !range.contains(ruler.position)
        seekOffset(range.l)
      event.stopPropagation()

    $scope.select = (event, c) ->
      return false if $scope.editing == 'position'
      if !c || $scope.current == c
        seekPosition event.clientX
      else
        select(c)
      return

    $scope.setSelection = (pos, u) ->
      if u == undefined
        ruler.selection = new Segment(pos)
      else
        sel = ruler.selection
        sel = slot.segment if sel.empty
        ruler.selection =
          if u
            new Segment(Math.min(sel.l, pos), pos+0.1)
          else
            new Segment(pos, Math.max(sel.u, pos+0.1))
      finalizeSelection()
      return

    $scope.dragSelection = (down, up, c) ->
      return false if $scope.editing == 'position' || c && $scope.current != c

      startPos = down.position ?= positionOffset(down.clientX)
      endPos = positionOffset(up.clientX)
      ruler.selection =
        if startPos < endPos
          new Segment(startPos, endPos)
        else if startPos > endPos
          new Segment(endPos, startPos)
        else if startPos = endPos
          new Segment(startPos)
        else
          new Segment(null)
      finalizeSelection() if up.type != 'mousemove'
      return

    $scope.zoom = (seg) ->
      seg ?= fullRange
      ruler.range = seg
      ruler.zoomed = seg != fullRange
      searchLocation($location.replace())
      return

    $scope.updateSelection = finalizeSelection = ->
      if editing
        return false if $scope.editing == 'position'
        $scope.editing = true
        $scope.current.updateExcerpt() if $scope.current?.excerpts
      for t in $scope.tags
        t.update()
      return

    getSelection = ->
      if ruler.selection.empty then new Segment(ruler.position) else ruler.selection

    $scope.addBlank = () ->
      unless blank
        $scope.tracks.push(blank = new Track())
      select(blank)
      blank

    removed = (track) ->
      return if track.asset || track.file
      select() if track == $scope.current
      blank = undefined if track == blank
      $scope.tracks.remove(track)
      return

    class Track extends Store
      constructor: (asset) ->
        super slot, asset
        @excerpts = []
        return

      type: 'asset'

      setAsset: (asset) ->
        super asset
        return unless asset
        @segment = new Segment(asset.segment)
        select(this) if `asset.id == target.asset`
        $scope.asset = asset if $scope.current == this
        return

      Object.defineProperty @prototype, 'id',
        get: -> @asset?.id

      remove: ->
        r = super()
        return removed this unless r?.then
        r.then (done) =>
          removed this if done
          return
        return

      save: ->
        super().then (done) =>
          return unless done
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.current
          updateRange()
          sortTracks()
          return

      upload: (file) ->
        blank = undefined if this == blank
        super(file).then (done) =>
          return removed this unless done
          ### jshint ignore:start ###
          @data.name ||= file.file.name
          ### jshint ignore:end ###
          return
        return

      rePosition: () ->
        $scope.editing = 'position'
        return

      updatePosition: () ->
        @segment.u = @segment.l + @asset.duration
        return

      setPosition: (p) ->
        @segment.l = p
        @updatePosition()
        $scope.form.position.$setDirty()
        return

      finishPosition: () ->
        $scope.form.position.$setPristine()
        @segment = new Segment(@asset.segment)
        $scope.editing = true
        return

      savePosition: () ->
        messages.clear(this)
        shift = @asset?.segment.l
        @asset.save({container:slot.id, position:Math.floor(@segment.l)}).then (asset) =>
            @asset = asset
            shift -= @asset.segment.l
            if shift
              for e in @excerpts
                e.segment.l -= shift
                e.segment.u -= shift
            updateRange()
            sortTracks()
            @finishPosition()
          , (res) =>
            @finishPosition()
            messages.addError
              type: 'red'
              body: constants.message('asset.update.error', @name)
              report: res
              owner: this
            return

      dragMove: (down, up) ->
        offset = down.offset ?= positionOffset(down.clientX) - @segment.l
        pos = positionOffset(up.clientX) - offset
        return unless isFinite(pos)
        @setPosition(pos)
        if up.type != 'mousemove'
          $scope.updatePosition()
        return

      updateExcerpt: () ->
        @excerpt = undefined
        seg = getSelection()
        return if !@asset || !seg || (@segment.full && !seg.full) || !@segment.overlaps(seg)
        excerpt = @excerpts.find((e) -> seg.overlaps(e.segment))
        @excerpt =
          if !excerpt
            target: @asset.inSegment(seg)
            on: false
            classification: ''
          else if excerpt.segment.equals(seg)
            target: excerpt
            on: true
            classification: excerpt.excerpt+''
          else
            null
        return

      editExcerpt: () ->
        @updateExcerpt() # should be unnecessary
        $scope.editing = 'excerpt'
        return

      excerptOptions: () ->
        opts = super()
        opts[@excerpt.classification] = 'prompt' unless @excerpt.classification of opts
        opts

      saveExcerpt: (value) ->
        $scope.editing = true
        if value == undefined || value == ''
          return
        messages.clear(this)
        @excerpt.target.setExcerpt(value)
          .then (excerpt) =>
              @excerpts.remove(excerpt)
              if 'excerpt' of excerpt
                @excerpts.push(excerpt)
              @updateExcerpt()
            , (res) =>
              messages.addError
                type: 'red'
                body: constants.message('asset.update.error', @name)
                report: res
                owner: this

      canRestore: () ->
        Store.removedAsset if editing && this == blank && Store.removedAsset?.volume.id == slot.volume.id

      restore: () ->
        Store.restore(slot).then (a) =>
          @setAsset(a)
          blank = undefined if this == blank
          return

    $scope.fileAdded = (file) ->
      $flow = file.flowObj
      (!$scope.current?.file && $scope.current || $scope.addBlank()).upload(file) if editing
      return

    $scope.fileSuccess = Store.fileSuccess
    $scope.fileProgress = Store.fileProgress

    fillExcerpts = ->
      tracks = {}
      for t in $scope.tracks when t.asset
        tracks[t.asset.id] = t
      for e in slot.excerpts
        t = tracks[e.id]
        t.excerpts.push(e) if t
      return

    playerMinHeight = 200
    viewportMinHeight = 120
    playerHeight = parseInt(storage.get('player-height'), 10) || 400
    ### jshint ignore:start ###
    unless playerHeight >= playerMinHeight
      playerHeight = playerMinHeight
    ### jshint ignore:end ###

    playerImgHeightStyle = undefined
    playerVideoHeightStyle = undefined
    updatePlayerHeight = () ->
      $timeout ->
        player = document.getElementById('player-scroll')
        return unless player
        viewer = document.getElementById('player-viewport')
        d = player.offsetTop + playerHeight - viewer.offsetTop - 10 # viewport padding+border
        if d < viewportMinHeight
          d = viewportMinHeight
        playerImgHeightStyle = styles.set('.player-viewport .asset-display img{max-height:'+d+'px}', playerImgHeightStyle)
        playerVideoHeightStyle = styles.set('.player-viewport .asset-display video,.player-drop{height:'+d+'px}', playerVideoHeightStyle)
        return
      return
    setPlayerHeight = () ->
      storage.set('player-height', playerHeight)
      $scope.playerHeight = playerHeight
      updatePlayerHeight()
      return
    setPlayerHeight()

    $scope.resizePlayer = (down, up) ->
      frame = document.getElementById('slot-player')
      frame.style.overflow = 'visible'
      player = document.getElementById('player-scroll')
      bar = down.currentTarget
      h = Math.max(player.offsetHeight + up.clientY - down.clientY, playerMinHeight)
      if up.type == 'mousemove'
        bar.style.top = h - player.offsetHeight + 'px'
      else
        frame.style.overflow = 'hidden'
        bar.style.top = '0px'
        playerHeight = h
        setPlayerHeight()
      return

    videoEvents =
      pause: ->
        $scope.playing = 0
        return
      playing: ->
        $scope.playing = 1
        return
      ratechange: ->
        $scope.playing = video[0].playbackRate
        return
      timeupdate: ->
        if $scope.asset && isFinite($scope.asset.segment.l)
          o = 1000*video[0].currentTime
          if $scope.editing == 'position' && $scope.asset == $scope.current.asset
            $scope.current.setPosition(ruler.position - o)
          else
            ruler.position = $scope.asset.segment.l + o
            if ruler.selection.uBounded && ruler.position >= ruler.selection.u
              video[0].pause()
              seekOffset(ruler.selection.l) if ruler.selection.lBounded
        return
      ended: ->
        $scope.playing = 0
        # look for something else to play?
        return

    for ev, fn of videoEvents
      videoEvents[ev] = $scope.$lift(fn)

    @deregisterVideo = (v) ->
      return unless video == v
      video = undefined
      v.off(videoEvents)
      return

    @registerVideo = (v) ->
      this.deregisterVideo video if video
      video = v
      seekOffset(ruler.position)
      v.on(videoEvents)
      return

    class Record
      constructor: (r) ->
        @rec = r
        @record = r.record || slot.volume.records[r.id]
        for f in ['age'] when f of r
          @[f] = r[f]
        @segment = new Segment(r.segment)
        if editing
          @fillData()
        return

      type: 'record'

      fillData: ->
        @data =
          measures: angular.extend({}, @record.measures)
          add: ''
        return

      Object.defineProperty @prototype, 'id',
        get: -> @rec.id

      remove: ->
        messages.clear(this)
        slot.removeRecord(@rec, @segment).then (r) =>
            return unless r
            records.remove(this)
            select() if $scope.current == this
            placeRecords()
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: 'Unable to remove'
              report: res
              owner: this
            return

      ### jshint ignore:start #### fixed in jshint 2.5.7
      metrics: ->
        ident = constants.category[@record.category]?.ident || [constants.metricName.ident.id]
        (constants.metric[m] for m of @record.measures when !(+m in ident)).sort(byId)

      addMetric = {id:'',name:'Add new value...'}
      addOptions: ->
        metrics = (metric for m, metric of constants.metric when !(m of @data.measures)).sort(byId)
        metrics.unshift addMetric
        metrics
      ### jshint ignore:end ###

      add: ->
        @data.measures[@data.add] = '' if @data.add
        @data.add = ''
        return

      save: ->
        messages.clear(this)
        @record.save({measures:@data.measures}).then () =>
            @fillData()
            delete @dirty
            if this == $scope.current
              $scope.form.edit.$setPristine()
              $scope.form.measures.$setPristine()
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving record'
              report: res
              owner: this
            return

      rePosition: () ->
        $scope.editing = 'position'
        return

      updatePosition: () ->
        return

      finishPosition: () ->
        $scope.editing = true
        @segment = new Segment(@rec.segment)
        $scope.form.position.$setPristine()
        return

      savePosition: () ->
        messages.clear(this)
        slot.moveRecord(@rec, @rec.segment, @segment).then (r) =>
            return unless r # nothing happened
            if @segment.empty
              records.remove(this)
              select() if this == $scope.current
            @finishPosition()
            placeRecords()
            updateRange()
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving record'
              report: res
              owner: this
            return

      dragLeft: (event) ->
        @segment.l = positionOffset(event.clientX)
        if event.type != 'mousemove'
          $scope.form.position.$setDirty()
        return

      dragRight: (event) ->
        @segment.u = positionOffset(event.clientX)
        if event.type != 'mousemove'
          $scope.form.position.$setDirty()
        return

    placeRecords = () ->
      records.sort (a, b) ->
        a.record.category - b.record.category || a.record.id - b.record.id
      t = []
      overlaps = (rr) -> rr.record.id != r.record.id && s.overlaps(rr.segment)
      for r in records
        s = r.segment
        for o, i in t
          break unless o[0].record.category != r.record.category || o.some(overlaps)
        t[i] = [] unless i of t
        t[i].push(r)
        select(r) if `r.id == target.record`
      for r in t
        r.sort byPosition
      $scope.records = t
      return

    $scope.setCategory = (c) ->
      if c?
        rs = {}
        for ri, r of $scope.addRecord.records when `(r.category || 0) == c`
          rs[ri] = r.displayName
        $scope.addRecord.options = rs
      else
        $scope.addRecord.options = undefined
      return

    $scope.addRecord = (r) ->
      seg = getSelection()
      if r == undefined
        $scope.editing = 'record'
        rs = {}
        for ri, r of slot.volume.records
          rs[ri] = r
        for sr in records
          if sr.record.id of rs && sr.segment.overlaps(seg)
            delete rs[sr.record.id]
        $scope.addRecord.records = rs
        rc = {}
        for ri, r of rs
          rc[r.category || 0] = null
        $scope.addRecord.categories = rc
        $scope.addRecord.select = null
        $scope.setCategory($scope.addRecord.category)
      else
        $scope.addRecord.records = undefined
        unless r?
          $scope.editing = true
          return
        slot.addRecord(slot.volume.records[r], seg).then (rec) ->
            $scope.editing = true
            r = new Record
              id: rec.id
              record: rec
              segment: seg
            records.push(r)
            placeRecords()
            select(r)
            return
          , (res) ->
            $scope.editing = true
            messages.addError
              body: 'Error adding record'
              report: res
              owner: $scope
            return
      return

    $scope.positionBackgroundStyle = (l, i) ->
      $scope.positionStyle(new Segment(l[i].segment.l, if i+1 of l then l[i+1].segment.l else Infinity))

    class Consent
      constructor: (c) ->
        if typeof c == 'object'
          @consent = c.consent
          @segment = Segment.make(c.segment)
        else
          @consent = c
          @segment = Segment.full
        return

      type: 'consent'

      classes: ->
        cn = constants.consent[@consent]
        cls = [cn, 'hint-consent-' + cn]
        cls.push('slot-consent-select') if $scope.current == this
        cls

    class TagName
      constructor: (name) ->
        @id = name
        return

      save: (vote) ->
        seg = getSelection()
        tag = this
        slot.setTag(@id, vote, editing, seg).then (data) ->
            unless tag instanceof Tag
              tag = $scope.tags.find (t) -> t.id == data.id
              unless tag
                tag = new Tag(data)
                tag.active = true
                $scope.tags.push(tag)
            tag.fillData(data)
            if (if editing then tag.keyword?.length else tag.weight)
              tag.update()
            else
              $scope.tags.remove(tag)
            tooltips.clear() # hack for broken tooltips
            return
          , (res) =>
            messages.addError
              body: constants.message('tags.vote.error', {sce: $sce.HTML}, @id)
              report: res
              owner: $scope
            return

    $scope.vote = (name, vote) ->
      new TagName(name).save(vote)

    class Tag extends TagName
      constructor: (t) ->
        @id = t.id
        @active = false
        @fillData(t)
        return

      type: 'tag'

      fillData: (t) ->
        @weight = t.weight
        for f in (if editing then ['keyword'] else ['coverage','vote','keyword'])
          this[f] = []
          if t[f]
            for s in t[f]
              this[f].push(Segment.make(s))
        return

      toggle: ->
        @active = !@active

      update: ->
        state = false
        sel = getSelection()
        for s in (if editing then @keyword else @vote)
          if sel.contains(s)
            state = true
          else if sel.overlaps(s)
            state = undefined
            break
        @state = state

    class Comment
      constructor: (c) ->
        @comment = c
        @segment = new Segment(c.segment)

      type: 'comment'

      getClass: ->
        cls = []
        if @comment.parents
          cls.push('depth-'+Math.min(@comment.parents.length, 5))
        cls

    ### jshint ignore:start #### fixed in jshint 2.5.7
    $scope.tags = (new Tag(tag) for tagId, tag of slot.tags when (if editing then tag.keyword?.length else tag.weight))
    $scope.tracks = (new Track(asset) for assetId, asset of slot.assets)
    $scope.comments = (new Comment(comment) for comment in slot.comments)
    ### jshint ignore:end ###
    sortTracks()
    fillExcerpts()

    records = slot.records.map((r) -> new Record(r))

    $scope.consents =
      if Array.isArray(consents = slot.consents)
        consents.map((c) -> new Consent(c))
      else if (consents)
        [new Consent(consents)]
      else
        []

    $scope.playing = 0
    placeRecords()
    updateRange()
    finalizeSelection()

    if editing
      done = $scope.$on '$locationChangeStart', (event, url) ->
        return if url.includes(slot.editRoute())
        if $flow
          uploading = $flow.isUploading()
          $flow.pause()
        if stayDirty(uploading)
          $flow.resume() if uploading
          display.cancelRouteChange(event)
        else
          done()
        return

    $scope.$on '$destroy', ->
      $flow?.cancel()
      return

    return
])
