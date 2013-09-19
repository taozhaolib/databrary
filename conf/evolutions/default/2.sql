-- This populates some demo data to help in testing
-- It should probably not be used in production, but could be.

# --- !Ups
;

INSERT INTO party (id, name, orcid) VALUES (1, 'Dylan Simon', '0000000227931679');
INSERT INTO party (id, name) VALUES (2, 'Mike Continues');
INSERT INTO party (id, name) VALUES (3, 'Lisa Steiger');
INSERT INTO party (id, name) VALUES (4, 'Karen Adolph');
INSERT INTO party (id, name) VALUES (5, 'Rick Gilmore');
SELECT setval('party_id_seq', 6);

INSERT INTO account (id, email, openid) VALUES (1, 'dylan@databrary.org', 'http://dylex.net/');
INSERT INTO account (id, email, openid) VALUES (2, 'mike@databrary.org', NULL);
INSERT INTO account (id, email, openid) VALUES (3, 'lisa@databrary.org', NULL);

INSERT INTO authorize (child, parent, access, delegate) VALUES (1, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (2, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, access, delegate) VALUES (3, 0, 'CONTRIBUTE', 'NONE');

INSERT INTO volume (id, name, body) VALUES (1, 'Demo sandbox', 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus imperdiet in nunc non porttitor. Nunc dictum dolor vitae mi ultricies, nec pharetra libero semper. Etiam vel velit auctor, bibendum nisi in, mollis diam. Nunc adipiscing et turpis eget laoreet. Donec accumsan ut odio eu gravida. Proin tristique sed neque sit amet blandit. Fusce quis dapibus nisi, non porta dui. Etiam posuere congue elit, at dignissim risus venenatis vitae. Duis cursus tincidunt quam ut ultricies. Aliquam erat volutpat. Aliquam accumsan malesuada metus non feugiat. In molestie interdum nibh eu pharetra.

Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vivamus viverra luctus neque fringilla sagittis. Donec non elementum elit. Suspendisse vitae varius augue. Donec sodales felis vel nulla ornare tristique eget sed quam. Morbi eget dolor ut urna luctus pulvinar. Mauris mollis lobortis sollicitudin. In nisl arcu, aliquet in mi ac, rhoncus faucibus ligula. Sed lacus felis, interdum ut rutrum ac, iaculis vitae arcu.

Quisque nec pulvinar sapien. Nulla diam nibh, egestas ac nulla et, interdum porttitor risus. Nam vitae rutrum dolor. Mauris in mauris nisi. Nunc ut pellentesque mauris. Aenean vitae lacinia erat. In pulvinar scelerisque libero, ac elementum elit condimentum a. Vivamus consequat purus lorem, vel adipiscing nibh pretium non. Quisque nec metus eget nulla pellentesque malesuada. Praesent eu accumsan libero, a dapibus metus. Nulla sit amet tristique ligula. In sagittis consectetur arcu sit amet ornare. Phasellus sollicitudin dolor lectus, nec convallis est mattis vel. Nunc molestie dolor vel diam sodales euismod. Nunc viverra nibh eros, ac tristique lacus facilisis in.');
SELECT setval('volume_id_seq', 1);

INSERT INTO container (id, volume, name) VALUES (1, 1, 'Container');
SELECT setval('container_id_seq', 1);

INSERT INTO toplevel_slot (slot) VALUES (1);

INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, -1, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 0, 'DOWNLOAD', 'DOWNLOAD');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, access, inherit) VALUES (1, 2, 'ADMIN', 'NONE');

INSERT INTO timeseries (id, format, classification, duration) VALUES (1, -800, 'MATERIAL', interval '40');
SELECT setval('asset_id_seq', 1);

INSERT INTO container_asset (container, asset, position, name) VALUES (1, 1, '0', 'counting');

# --- !Downs
;

TRUNCATE party, volume, container, timeseries, asset CASCADE;
SELECT setval('party_id_seq', 1, false);
SELECT setval('container_id_seq', 1, false);
SELECT setval('slot_id_seq', 1, false);
SELECT setval('volume_id_seq', 1, false);
SELECT setval('asset_id_seq', 1, false);

INSERT INTO party VALUES (-1, 'Everybody');
INSERT INTO party VALUES (0, 'Databrary');
INSERT INTO authorize (child, parent, access, delegate) VALUES (0, -1, 'ADMIN', 'ADMIN');

