
ALTER TABLE atlas_app.person ADD COLUMN share_emergency_contacts boolean NOT NULL default false,
  ADD COLUMN trigger_ny_support boolean NOT NULL default true,
  ADD COLUMN night_time_safety boolean NOT NULL default true,
  ADD COLUMN has_completed_safety_setup boolean NOT NULL default false;

