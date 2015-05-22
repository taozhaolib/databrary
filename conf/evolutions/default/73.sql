# --- !Ups

ALTER TABLE "account_token" ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week';
ALTER TABLE "session" ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week';

# --- !Downs

ALTER TABLE "account_token" ALTER "expires" DROP DEFAULT;
ALTER TABLE "login_token" ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week';
ALTER TABLE "session" ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '4 weeks';
ALTER TABLE "upload" ALTER "expires" SET DEFAULT CURRENT_TIMESTAMP + interval '1 week';
