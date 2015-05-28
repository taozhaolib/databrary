-- This needs root and likely will have to be applied manually:
UPDATE pg_enum SET enumlabel = 'numeric' WHERE enumtypid = 'data_type'::regtype::oid AND enumlabel = 'number';
