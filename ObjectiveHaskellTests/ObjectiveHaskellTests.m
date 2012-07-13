//
//  ObjectiveHaskellTests.m
//  ObjectiveHaskellTests
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#import "ObjectiveHaskellTests.h"
#import "FibTest_stub.h"

@implementation ObjectiveHaskellTests

- (void)setUp {
    [super setUp];

    hs_init(NULL, NULL);
}

- (void)tearDown {
    hs_exit();

    [super tearDown];
}

- (void)testFibonacci {
    STAssertEquals(3, fibonacci_hs(4), @"");
    STAssertEquals(5, fibonacci_hs(5), @"");
}

@end
