!>@file   t_map_patch_from_1patch.f90
!!@brief  module t_map_patch_from_1patch
!!
!!@author  H. Matsui
!!@date Programmed in May, 2023
!
!>@brief Divided triangle patch for map projection
!!
!!@verbatim
!!      subroutine alloc_map_patch_from_1patch(ntot_comp, map_e)
!!      subroutine dealloc_map_patch_from_1patch(map_e)
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!
!!      subroutine set_sph_position_4_map_patch                         &
!!     &         (n_map_patch, x_map_patch, rtp_map_patch)
!!        integer(kind = kint), intent(in) :: n_map_patch
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: x_map_patch(num_triangle,n_vector,n_map_patch)
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: rtp_map_patch(num_triangle,n_vector,n_map_patch)
!!      subroutine patch_to_aitoff(n_map_patch, rtp_map_patch, xy_map)
!!        integer(kind = kint), intent(in) :: n_map_patch
!!        real(kind = kreal), intent(in)                                &
!!     &          :: rtp_map_patch(num_triangle,n_vector,n_map_patch)
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: xy_map(2,num_triangle,n_map_patch)
!!
!!      subroutine find_map_path_orientation                            &
!!     &         (xy_map, k_ymin, k_ymid, k_ymax, k_xmin, k_xmax)
!!        real(kind = kreal), intent(in) :: xy_map(2,num_triangle)
!!        integer(kind = kint), intent(inout) :: k_ymin, k_ymid, k_ymax
!!        integer(kind = kint), intent(inout) :: k_xmin, k_xmax
!!@endverbatim
      module t_map_patch_from_1patch
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_geometry_constants
!
      implicit none
!
      real(kind = kreal), parameter, private :: EPSILON = 1.0d-9
!
      integer(kind = kint), parameter :: nmax_map_p = 3
!
      type map_patches_for_1patch
        integer(kind = kint) :: n_map_patch
!
        real(kind=kreal), allocatable :: xy_map(:,:,:)
        real(kind=kreal), allocatable :: d_map_patch(:,:,:)
!
        real(kind=kreal), allocatable :: x_map_patch(:,:,:)
        real(kind=kreal), allocatable :: rtp_map_patch(:,:,:)
      end type map_patches_for_1patch
!
!      private :: nmax_map_p, x_map_patch, rtp_map_patch
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_map_patch_from_1patch(ntot_comp, map_e)
!
      integer(kind = kint), intent(in) :: ntot_comp
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      allocate(map_e%xy_map(2,num_triangle,nmax_map_p))
      allocate(map_e%d_map_patch(num_triangle,ntot_comp,nmax_map_p))
      allocate(map_e%x_map_patch(num_triangle,n_vector,nmax_map_p))
      allocate(map_e%rtp_map_patch(num_triangle,n_vector,nmax_map_p))
!
      map_e%xy_map(1:2,1:num_triangle,1:nmax_map_p) =  zero
      map_e%d_map_patch(1:num_triangle,1:ntot_comp,1:nmax_map_p) = zero
      map_e%x_map_patch(1:num_triangle,1:n_vector,1:nmax_map_p) =  zero
      map_e%rtp_map_patch(1:num_triangle,1:n_vector,1:nmax_map_p)= zero
!
      end subroutine alloc_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_map_patch_from_1patch(map_e)
!
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      deallocate(map_e%xy_map)
      deallocate(map_e%d_map_patch, map_e%x_map_patch)
      deallocate(map_e%rtp_map_patch)
!
      end subroutine dealloc_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_sph_position_4_map_patch                           &
     &         (n_map_patch, x_map_patch, rtp_map_patch)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: n_map_patch
      real(kind = kreal), intent(inout)                                 &
     &          :: x_map_patch(num_triangle,n_vector,n_map_patch)
      real(kind = kreal), intent(inout)                                 &
     &          :: rtp_map_patch(num_triangle,n_vector,n_map_patch)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: y_center(3)
      real(kind = kreal) :: ar_map(3), rs_map(3), as_map(3)
      real(kind = kreal) :: pi, xflag, yflag
!
!
      do i = 1, n_map_patch
        if(abs(x_map_patch(1,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
        if(abs(x_map_patch(2,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
        if(abs(x_map_patch(3,2,i)) .lt. EPSILON) x_map_patch(1,2,i)= 0.
      end do
!
      do i = 1, n_map_patch
        y_center(i) = (x_map_patch(1,2,i) + x_map_patch(2,2,i)          &
     &               + x_map_patch(3,2,i) ) / three
      end do
!
      do i = 1, n_map_patch
        call position_2_sph(ithree, x_map_patch, rtp_map_patch(1,1,i),  &
     &      rtp_map_patch(1,2,i), rtp_map_patch(1,3,i),                 &
     &      ar_map(1), rs_map(1), as_map(1))
      end do
!
      pi = four * atan(one)
      do i = 1, n_map_patch
        xflag = x_map_patch(1,1,i) + x_map_patch(2,1,i)                 &
     &         + x_map_patch(2,1,i)
        yflag = x_map_patch(1,2,i) * x_map_patch(2,2,i)                 &
     &         * x_map_patch(2,2,i)
!
        if(yflag.eq.zero .and. xflag.le.zero) then
          if( y_center(i) .gt. zero) then
            if(rtp_map_patch(1,3,i) .eq. zero) then
               rtp_map_patch(1,3,i) = two * pi
            end if
            if(rtp_map_patch(2,3,i) .eq. zero) then
               rtp_map_patch(2,3,i) = two * pi
            end if
            if(rtp_map_patch(3,3,i) .eq. zero) then
               rtp_map_patch(3,3,i) = two * pi
            end if
          end if
!
        end if
      end do
!
      end subroutine set_sph_position_4_map_patch
!
!-----------------------------------------------------------------------
!
      subroutine patch_to_aitoff(n_map_patch, rtp_map_patch, xy_map)
!
      use aitoff
!
      integer(kind = kint), intent(in) :: n_map_patch
      real(kind = kreal), intent(in)                                    &
     &          :: rtp_map_patch(num_triangle,n_vector,n_map_patch)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: xy_map(2,num_triangle,n_map_patch)
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: s_theta, c_theta, pi, phi_map
!
!
      pi = four * atan(one)
      do i = 1, n_map_patch
        do j = 1, num_triangle
          s_theta = sin(rtp_map_patch(j,2,i))
          c_theta = cos(rtp_map_patch(j,2,i))
          phi_map = mod((rtp_map_patch(j,3,i)+pi),(two*pi))
          call s_aitoff(s_theta, c_theta, phi_map,                      &
     &                  xy_map(1,i,j), xy_map(2,i,j))
        end do
      end do
!
      end subroutine patch_to_aitoff
!
!-----------------------------------------------------------------------
!
      subroutine find_map_path_orientation                              &
     &         (xy_map, k_ymin, k_ymid, k_ymax, k_xmin, k_xmax)
!
      real(kind = kreal), intent(in) :: xy_map(2,num_triangle)
      integer(kind = kint), intent(inout) :: k_ymin, k_ymid, k_ymax
      integer(kind = kint), intent(inout) :: k_xmin, k_xmax
!
!
      if(      xy_map(2,1) .le. xy_map(2,2)                             &
     &   .and. xy_map(2,1) .le. xy_map(2,3)) then
        k_ymin = 1
        if(xy_map(2,2) .le. xy_map(2,3)) then
          k_ymid = 2
          k_ymax = 3
        else
          k_ymid = 3
          k_ymax = 2
        end if
!
        if(xy_map(1,2) .le. xy_map(1,3)) then
          k_xmin = 2
          k_xmax = 3
        else
          k_xmin = 3
          k_xmin = 2
        end if
!
      else if( xy_map(2,2) .le. xy_map(2,3)                             &
     &   .and. xy_map(2,2) .le. xy_map(2,1)) then
        k_ymin = 2
        if(xy_map(2,3) .le. xy_map(2,1)) then
          k_ymid = 3
          k_ymax = 1
        else
          k_ymid = 1
          k_ymax = 3
        end if
!
        if(xy_map(1,3) .le. xy_map(1,1)) then
          k_xmin = 3
          k_xmax = 1
        else
          k_xmin = 1
          k_xmin = 3
        end if
!
      else
        k_ymin = 3
        if(xy_map(2,1) .le. xy_map(2,2)) then
          k_ymid = 1
          k_ymax = 2
        else
          k_ymid = 2
          k_ymax = 1
        end if
!
        if(xy_map(1,1) .le. xy_map(1,2)) then
          k_xmin = 1
          k_xmax = 2
        else
          k_xmin = 2
          k_xmin = 1
        end if
      end if
!
      end subroutine find_map_path_orientation
!
!-----------------------------------------------------------------------
!
      end module t_map_patch_from_1patch
