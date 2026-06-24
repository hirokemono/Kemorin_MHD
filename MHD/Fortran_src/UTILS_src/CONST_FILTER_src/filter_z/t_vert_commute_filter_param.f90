!>@file   t_vert_commute_filter_param.f90
!!        module t_vert_commute_filter_param
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter in vertical direction
!!
!!@verbatim
!!      subroutine write_vert_plane_filter_param(id_file, zfil_param)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(vert_commute_filter_param), intent(in) :: zfil_param
!!      subroutine read_vert_plane_filter_param(id_file, zfil_param)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(vert_commute_filter_param), intent(inout) :: zfil_param
!!
!!      subroutine set_vert_plane_filter_param(internal_node,           &
!!     &                                       zfil_param)
!!        integer(kind = kint), intent(in) :: internal_node
!!        type(vert_commute_filter_param), intent(inout) :: zfil_param
!!@endverbatim
!!
      module t_vert_commute_filter_param
!
      use m_precision
!
      implicit none
!
!
      character(len = kchara), parameter :: hd_tophat =   'Tophat'
      character(len = kchara), parameter :: hd_Linear =   'Linear'
      character(len = kchara), parameter :: hd_Gaussian = 'Gaussian'
!
      integer(kind = kint), parameter :: id_tophat =   0
      integer(kind = kint), parameter :: id_Linear =   1
      integer(kind = kint), parameter :: id_Gaussian = 2
!
      type vert_commute_filter_param
!>        File prefix for vertical filter file
        character(len=kchara) :: filter_z_file_prefix = 'filter_node_l.0'
!>        Vertical grid type
        integer (kind = kint) :: iflag_zgrid
!
!>        Number of node in x-direction
        integer(kind = kint) :: totalnod_x
!>        Number of node in y-direction
        integer(kind = kint) :: totalnod_y
!>        Number of node in vertical direction
        integer(kind = kint) :: totalnod_z
!>        Number of element in vertical direction
        integer(kind = kint) :: totalele
!
!>        domain size in x-direction
        real(kind = kreal) :: xsize
!>        domain size in y-direction
        real(kind = kreal) :: ysize
!>        domain size in z-direction
        real(kind = kreal) :: zsize
!
!>        number of filter
        integer(kind = kint) :: numfilter
!
        integer(kind = kint) :: num_filter_h
        character(len=kchara) :: type_filter_h
        integer(kind = kint) :: iflag_filter_h
        real(kind = kreal) :: f_width_h
!
        integer(kind = kint) :: num_filter_z
        character(len=kchara) :: type_filter_z
        integer(kind = kint) :: iflag_filter_z
        real(kind = kreal) :: f_width_z
!
        integer (kind = kint) :: i_int_z_filter
!
        integer (kind = kint) :: ncomp_mat
!
        integer(kind = kint) :: nfilter2_3
        integer(kind = kint) :: nfilter2_1
      end type vert_commute_filter_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vert_plane_filter_param(id_file, zfil_param)
!
      integer(kind = kint), intent(in) :: id_file
      type(vert_commute_filter_param), intent(in) :: zfil_param
!
!
      write(id_file,'(a)') '! number of node'
      write(id_file,'(3i6)') zfil_param%totalnod_x,                     &
     &                       zfil_param%totalnod_y,                     &
     &                       zfil_param%totalnod_z
      write(id_file,'(a)') '! size of domain'
      write(id_file,'(1p3E25.15e3)') zfil_param%xsize,                  &
     &                               zfil_param%ysize,                  &
     &                               zfil_param%zsize
!
      write(id_file,'(a)') '!grid type'
      write(id_file,'(a)') '!   0:equally divided'
      write(id_file,'(a)') '!   1:Chebycyev points from 0 to pi/2'
      write(id_file,'(a)') '!   2:Chebycyev points from 0 to pi'
      write(id_file,'(i3)') zfil_param%iflag_zgrid
!
      end subroutine write_vert_plane_filter_param
!
!  ---------------------------------------------------------------------
!
      subroutine read_vert_plane_filter_param(id_file, zfil_param)
!
      integer(kind = kint), intent(in) :: id_file
      type(vert_commute_filter_param), intent(inout) :: zfil_param
!
      character(len = kchara) :: tmpchara
!
!
      read(id_file,*) tmpchara
      read(id_file,*) zfil_param%totalnod_x, zfil_param%totalnod_y,     &
     &                zfil_param%totalnod_z
      read(id_file,*) tmpchara
      read(id_file,*) zfil_param%xsize, zfil_param%ysize,               &
     &                zfil_param%zsize
!
      read(id_file,*) tmpchara
      read(id_file,*) tmpchara
      read(id_file,*) tmpchara
      read(id_file,*) tmpchara
      read(id_file,*) zfil_param%iflag_zgrid
!
      end subroutine read_vert_plane_filter_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function set_z_filter_type_id(type_filter)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: type_filter
!
      if      (cmp_no_case(type_filter, hd_tophat)) then
       set_z_filter_type_id = id_tophat
      else if (cmp_no_case(type_filter, hd_Linear)) then
       set_z_filter_type_id = id_Linear
!      else if (cmp_no_case(type_filter, hd_Gaussian)) then
      else
       set_z_filter_type_id = id_Gaussian
      end if
!
      end function set_z_filter_type_id
!
!  ---------------------------------------------------------------------
!
      character(len = kchara) function                                  &
     &                       set_z_filter_type_name(id_filter)
!
      integer(kind = kint), intent(in) :: id_filter
!
      if      (id_filter .eq. id_tophat) then
       set_z_filter_type_name = hd_tophat
      else if(id_filter .eq. id_Linear) then
       set_z_filter_type_name = hd_Linear
!      else if(id_filter .eq. id_Gaussian) then
      else
       set_z_filter_type_name = hd_Gaussian
      end if
!
      end function set_z_filter_type_name
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_vert_plane_filter_param(internal_node,             &
     &                                       zfil_param)
!
      integer(kind = kint), intent(in) :: internal_node
      type(vert_commute_filter_param), intent(inout) :: zfil_param
!
!
      zfil_param%totalele =   internal_node - 1
!
      zfil_param%nfilter2_1 = 2 * zfil_param%numfilter + 1
      zfil_param%nfilter2_3 = 2 * zfil_param%numfilter + 3
!
      end subroutine set_vert_plane_filter_param
!
!  ---------------------------------------------------------------------
!
      end module t_vert_commute_filter_param
