#!/usr/bin/perl -w

use strict;
use warnings;

print("Making sure svn working copy is up to date...\n");
system("cd ../svn-mojoshader ; svn update");
my $svnver = `cd ../svn-mojoshader ; svnversion`;
chomp($svnver);
my $hgver = `cd ../svn-mojoshader ; svn log -r${svnver} |grep 'changeset:'`;
chomp($hgver);
$hgver =~ s/^changeset:\s+(\d+):.*\Z/$1/;
my $min = $hgver;
$hgver = `hg tip |grep 'changeset:'`;
chomp($hgver);
$hgver =~ s/^changeset:\s+(\d+):.*\Z/$1/;
my $max = $hgver;

if ($min == $max) {
    print("We're already up to date. Exiting.\n");
    exit 0;
}

$min++;  # $min was the last committed revision, so we need to start one later.

print("first hg revision to commit to svn == '$min'\n");
print("last hg revision to commit to svn == '$max'\n");

my $i;
for ($i = $min; $i <= $max; $i++) {
    my $p = $i - 1;

    print("Revision #$i ...\n");
    print("Getting log from Mercurial...\n");
    system("hg log -v -r$i |grep -v '^tag: ' |tail -n +7 > commit.txt");
    system("echo >> commit.txt");
    system("echo 'This commit is from my temporary Mercurial repository...' >> commit.txt");
    system("hg log -v -r$i |grep -v '^tag: ' |head -n 4 >> commit.txt");
    print("Getting diff...\n");
    system("hg diff -r${p}:${i} > patch.diff");
    print("Patching svn working copy...\n");
    system("cd ../svn-mojoshader ; patch -p1 < ../mojoshader/patch.diff");
    print("Committing svn working copy...\n");
    system("cd ../svn-mojoshader ; svn commit -F ../mojoshader/commit.txt");
    print("Cleaning up...\n");
    system("rm -rf commit.txt patch.diff");
    print("...revision committed!\n");
}

print("svn push complete!\n\n");
exit 0;

