/* glock.c: Routines for locking critical sections
   Copyright (c) 2016 BogDan Vatra

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

   Author contact information:

   BogDan Vatra <bogdan@kde.org>
*/

#include <config.h>

#include <stdatomic.h>

void
atomic_lock( atomic_char *lock_ptr )
{
  char locked = ATOMIC_VAR_INIT( 1 );
  char unlocked;
  do {
    unlocked = ATOMIC_VAR_INIT( 0 );
  } while( !atomic_compare_exchange_strong( lock_ptr, &unlocked, locked ) );
}

void
atomic_unlock( atomic_char *lock_ptr )
{
  char locked;
  char unlocked = ATOMIC_VAR_INIT( 0 );
  do {
    locked = ATOMIC_VAR_INIT( 1 );
  } while( !atomic_compare_exchange_strong( lock_ptr, &locked, unlocked ) );
}
