//
//  UserViewModel.m
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
//

#import "UserViewModel.h"
#import "User.h"

@interface UserViewModel ()
@property (copy) User *user;
@end

@implementation UserViewModel
// TODO: These accessors can be dynamically generated.
- (NSString *)fullName {
	return self.user.fullName;
}

- (NSString *)username {
	return self.user.username;
}

- (NSURL *)photoURL {
	return self.user.photoURL;
}

- (id)initWithAccessToken:(NSString *)accessToken {
	self = [super init];
	if (self == nil) return nil;

	NSString *endpoint = [NSString stringWithFormat:@"https://api.instagram.com/v1/users/self?access_token=%@", accessToken];
	NSURLRequest *request = [NSURLRequest requestWithURL:[NSURL URLWithString:endpoint]];

	// TODO: This handler should be dispatched to a background queue.
	[NSURLConnection sendAsynchronousRequest:request queue:[NSOperationQueue mainQueue] completionHandler:^(NSURLResponse *response, NSData *data, NSError *error){
		if (data == nil) {
			[NSApp presentError:error];
			return;
		}

		self.user = [[User alloc] initWithData:data];
	}];

	return self;
}

+ (NSArray *)bindings {
	// TODO: This can be automated.
	return @[@"fullName", @"username", @"photoURL"];
}

+ (NSSet *)keyPathsForValuesAffectingValueForKey:(NSString *)key {
	// TODO: This can be automated.
	if ([self.bindings containsObject:key]) {
		return [NSSet setWithObject:@"user"];
	} else {
		return [super keyPathsForValuesAffectingValueForKey:key];
	}
}

@end
