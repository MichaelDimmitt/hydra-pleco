-- Fixture for the db-tests suite
insert into Users (userName, emailAddress, password)
values ('alice', 'alice@example.org', '');

insert into Projects (name, displayName, description, enabled, hidden, owner, homepage)
values
  ('pleco', 'Pleco', 'A visible, enabled project', 1, 0, 'alice', 'https://example.org'),
  ('hidden', 'Hidden', 'A hidden project', 1, 1, 'alice', null),
  ('paused', 'Paused', 'A disabled project', 0, 0, 'alice', null);

insert into Jobsets
  (id, project, name, description, type, flake, emailOverride, enabled, enableEmail, hidden)
values
  (1, 'pleco', 'main', 'The main jobset', 1, 'github:sgillespie/hydra-pleco', '', 1, 0, 0),
  (2, 'pleco', 'staging', null, 1, 'github:sgillespie/hydra-pleco/staging', '', 0, 0, 1);

insert into JobsetEvals
  (id, jobset_id, timestamp, checkoutTime, evalTime, hasNewBuilds, hash, nrBuilds, nrSucceeded, flake)
values
  (1, 1, 1700000000, 2, 3, 1, 'deadbeef', 5, 5, 'github:sgillespie/hydra-pleco'),
  (2, 1, 1700000600, 1, 2, 0, 'cafebabe', null, null, null);
