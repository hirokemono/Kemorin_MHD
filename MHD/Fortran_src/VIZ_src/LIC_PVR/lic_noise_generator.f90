!lic_noise_generator.f90
!
!      module lic_noise_generator
!
!      Written by Yangguang Liao 2018
!
!      subroutine import_noise_ary(filename, n_raw_data, n_data_size)
!
      module lic_noise_generator
!
      use m_precision
      use m_constants
      use binary_IO
      use calypso_mpi
!
      implicit  none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine import_noise_ary(filename, n_raw_data, n_data_size, ierr)

use set_parallel_file_name

! parameter for read noise data
character(len = kchara), intent(in) :: filename
integer(kind = kint), intent(inout) :: n_data_size(3)
character(len=1), allocatable, intent(inout) :: n_raw_data(:)
integer(kind = kint), intent(inout) :: ierr
integer(kind = kint) :: d_size
character(len=kchara) :: file_name
!
call add_null_character(filename, file_name)
call open_rd_rawfile(file_name, ierr)
if(ierr .eq. 0) then
! first line read 3 integer size data, byte 4
  call read_mul_integer_b(3, n_data_size)
  d_size = n_data_size(1)*n_data_size(2)*n_data_size(3)
  allocate( n_raw_data(d_size))  ! allocate space for noise data
  call read_mul_one_character_b(d_size, n_raw_data)
end if
close(16)


      end subroutine import_noise_ary
!
!  ---------------------------------------------------------------------
!

subroutine cal_pos_idx_volume(noise_size, xx_org, xyz_min, xyz_max, idx)

use set_parallel_file_name

!
integer(kind = kint), intent(in) :: noise_size
real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
integer(kind = kint), intent(inout) :: idx
integer(kind = kint) :: xyz_i(3)
!
integer(kind = kint) :: dim
real(kind = kreal) :: xyz_norm(3)
xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
dim = noise_size**(1./3.)
xyz_i = int(xyz_norm * dim)
xyz_i = mod((xyz_i * 2), dim)

idx = xyz_i(1) + (xyz_i(2)-1) * dim + (xyz_i(3)-1) * dim * dim

end subroutine cal_pos_idx_volume
!
!  ---------------------------------------------------------------------
!

function get_noise_value(noise_size, noise_data, idx)

use set_parallel_file_name

!
integer(kind = kint), intent(in) :: noise_size
character(len=1), intent(in) :: noise_data(noise_size)
integer(kind = kint), intent(in) :: idx
real(kind = kreal) :: get_noise_value

!
if(idx > noise_size) then
  get_noise_value = 0.0
  return
end if
get_noise_value = ichar(noise_data(idx)) / 255.0
!if(get_noise_value > 50.0) then
!  get_noise_value = 1.0
!else
!  get_noise_value = 0.0
!end if

end function get_noise_value
!
!  ---------------------------------------------------------------------
!
function get_offset_vol(x, y, z, noise_dim)
integer(kind=kint) :: get_offset_vol

integer(kind = kint) :: x, y, z, noise_dim

if(x .gt. noise_dim) x = noise_dim
if(x .lt. 1) x = 1
if(y .gt. noise_dim) y = noise_dim
if(y .lt. 1) y = 1
if(z .gt. noise_dim) z = noise_dim
if(z .lt. 1) z = 1
get_offset_vol = x + (y-1)*noise_dim + (z-1)*noise_dim*noise_dim

end function get_offset_vol
!
!  ---------------------------------------------------------------------
!
subroutine noise_sampling(noise_size, noise_data, xx_org, xyz_min, xyz_max, noise_value)

use set_parallel_file_name
!
integer(kind = kint), intent(in) :: noise_size
character(len=1), intent(in) :: noise_data(noise_size)
real(kind = kreal), intent(in) :: xx_org(3), xyz_min(3), xyz_max(3)
real(kind = kreal), intent(inout) :: noise_value
integer(kind = kint) :: idx, idx000,idx001,idx010,idx011,idx100,idx101,idx110,idx111
real(kind = kreal) :: xyz(3), xyz_d(3), c00, c01, c10, c11, c0, c1
integer(kind = kint) :: xyz_i(3)
!
integer(kind = kint) :: dim
real(kind = kreal) :: xyz_norm(3)

noise_value = 0.0
xyz_norm = (xx_org - xyz_min) / (xyz_max - xyz_min)
xyz_norm = xyz_norm * 4
xyz_norm = xyz_norm - int(xyz_norm)
dim = noise_size**(1./3.)
xyz = xyz_norm * dim
xyz_i = int(xyz_norm * dim + 0.5)
xyz_d = xyz + 0.5 - xyz_i
idx000 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3), dim)
idx001 = get_offset_vol(xyz_i(1), xyz_i(2), xyz_i(3)+1, dim)
idx010 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3), dim)
idx011 = get_offset_vol(xyz_i(1), xyz_i(2)+1, xyz_i(3)+1, dim)
idx100 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3), dim)
idx101 = get_offset_vol(xyz_i(1)+1, xyz_i(2), xyz_i(3)+1, dim)
idx110 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3), dim)
idx111 = get_offset_vol(xyz_i(1)+1, xyz_i(2)+1, xyz_i(3)+1, dim)
c00 = get_noise_value(noise_size, noise_data, idx000) * (1 - xyz_d(1))  &
&   + get_noise_value(noise_size, noise_data, idx100) * xyz_d(1)
c01 = get_noise_value(noise_size, noise_data, idx001) * (1 - xyz_d(1))  &
&   + get_noise_value(noise_size, noise_data, idx101) * xyz_d(1)
c10 = get_noise_value(noise_size, noise_data, idx010) * (1 - xyz_d(1))  &
&   + get_noise_value(noise_size, noise_data, idx110) * xyz_d(1)
c11 = get_noise_value(noise_size, noise_data, idx011) * (1 - xyz_d(1))  &
&   + get_noise_value(noise_size, noise_data, idx111) * xyz_d(1)

c0 = c00 * (1 - xyz_d(2)) + c10 * xyz_d(2)
c1 = c01 * (1 - xyz_d(2)) + c11 * xyz_d(2)
noise_value = c0 * (1 - xyz_d(3)) + c1 * xyz_d(3)

!xyz_i = mod((xyz_i * 2), dim)

end subroutine noise_sampling
!
!  ---------------------------------------------------------------------
!

      end module lic_noise_generator
