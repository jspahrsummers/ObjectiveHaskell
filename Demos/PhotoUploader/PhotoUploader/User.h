//
//  User.h
//  PhotoUploader
//
//  Created by Justin Spahr-Summers on 2012-08-03.
//  Released into the public domain.
//

@interface User : OHModel
@property (nonatomic, copy, readonly) NSString *fullName;
@property (nonatomic, copy, readonly) NSString *username;
@property (nonatomic, copy, readonly) NSURL *photoURL;
@end
