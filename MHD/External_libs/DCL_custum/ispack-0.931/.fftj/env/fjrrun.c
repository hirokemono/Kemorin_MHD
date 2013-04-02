/*
 * FTTJ:  An FFT library
 * Copyright (C) 2008 Keiichi Ishioka <ishioka@gfd-dennou.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
void fjrrun_(void *z1, void *z2, void *z3, void *z4, long *y)
{
   void (*x)(void *, void *, void *, void *);
     x=(void (*)())*y;
     (*x)(z1,z2,z3,z4);
     return;
}
