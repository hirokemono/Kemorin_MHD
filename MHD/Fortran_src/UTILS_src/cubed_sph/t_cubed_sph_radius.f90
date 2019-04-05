!
!      module t_cubed_sph_radius
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine set_cubed_sph_radius_ctl(cubed_sph_c, rprm_csph)
!!      subroutine set_cubed_rect_adjusting_ctl                         &
!!     &         (edge_latitude_ctl, rprm_csph)
!!      subroutine alloc_shell_radius(rprm_csph)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!!        type(ctl_array_ir), intent(in) :: edge_latitude_ctl
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!!
!!      subroutine dealloc_shell_radius(rprm_csph)
!!      subroutine dealloc_ref_edge_latitude(rprm_csph)
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!
      module t_cubed_sph_radius
!
      use m_precision
!
      implicit none
!
      type cubed_sph_radius
!>   num. of layer
        integer(kind = kint) :: n_shell
        integer(kind = kint) :: nr_adj
        integer(kind = kint) :: nr_back
!>   radius
        real(kind = kreal), allocatable :: r_nod(:)
!
        integer(kind = kint) :: nr_ocore
        integer(kind = kint) :: nr_exter
!
!  corner edge information
        integer(kind = kint) :: nedge_ref_lat
        integer(kind = kint), allocatable :: kr_edge_ref_lat(:)
        real(kind = kreal), allocatable :: edge_ref_lat(:)
        real(kind = kreal), allocatable :: edge_latitude(:)
      end type cubed_sph_radius
!
      private :: alloc_ref_edge_latitude
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_radius_ctl(cubed_sph_c, rprm_csph)
!
      use m_cubed_sph_grp_param
      use t_numref_cubed_sph
      use t_control_data_cubed_sph
      use skip_comment_f
      use set_cubed_sph_group_ctl
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
      integer(kind = kint) :: i, j
!
!
      rprm_csph%n_shell = cubed_sph_c%radial_pnt_ctl%num
!
      rprm_csph%nr_adj = 1
      if(cubed_sph_c%nend_adjust_ctl%iflag .gt. 0) then
        rprm_csph%nr_adj =  cubed_sph_c%nend_adjust_ctl%intvalue
      end if

      if(cubed_sph_c%nstart_cube_ctl%iflag .gt. 0) then
         rprm_csph%nr_back = cubed_sph_c%nstart_cube_ctl%intvalue
      else
         rprm_csph%nr_back = rprm_csph%n_shell
      end if
!
      call alloc_shell_radius(rprm_csph)
!
      do i = 1, rprm_csph%n_shell
        j = cubed_sph_c%radial_pnt_ctl%ivec(i)
        rprm_csph%r_nod(j) = cubed_sph_c%radial_pnt_ctl%vect(i)
      end do
!
      write(*,*) 'n_shell', rprm_csph%n_shell
      write(*,*) 'nr_adj',  rprm_csph%nr_adj
      write(*,*) 'nr_back', rprm_csph%nr_back
!
      end subroutine set_cubed_sph_radius_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_rect_adjusting_ctl                           &
     &         (edge_latitude_ctl, rprm_csph)
!
      use m_cubed_sph_grp_param
      use t_numref_cubed_sph
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_ir), intent(in) :: edge_latitude_ctl
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
      integer(kind = kint) :: j, k, kst, ked
!
!
      rprm_csph%nedge_ref_lat = edge_latitude_ctl%num
      write(*,*) 'nedge_ref_lat', rprm_csph%nedge_ref_lat
      call alloc_ref_edge_latitude(rprm_csph)
!
      if(rprm_csph%nedge_ref_lat .gt. 0) then
        do j = 1, rprm_csph%nedge_ref_lat
          rprm_csph%kr_edge_ref_lat(j) =  edge_latitude_ctl%ivec(j)
          rprm_csph%edge_ref_lat(j) = edge_latitude_ctl%vect(j)
        end do
      end if
!
!
      write(*,*) 'nedge_ref_lat', rprm_csph%nedge_ref_lat
      if(rprm_csph%nedge_ref_lat .gt. 0) then
        do j = 1, rprm_csph%nedge_ref_lat
          write(*,*) j, rprm_csph%kr_edge_ref_lat(j),                   &
     &                  rprm_csph%edge_ref_lat(j)
        end do
!
        ked = rprm_csph%kr_edge_ref_lat(1)
        do k = 1, ked
          rprm_csph%edge_latitude(k)                                    &
     &       = 45.0d0 + (rprm_csph%edge_ref_lat(1) - 45.0d0)            &
     &        * (rprm_csph%r_nod(k) - rprm_csph%r_nod(1))               &
     &        / (rprm_csph%r_nod(ked) - rprm_csph%r_nod(1))
        end do
!
        do j = 2, rprm_csph%nedge_ref_lat
          kst = rprm_csph%kr_edge_ref_lat(j-1)
          ked = rprm_csph%kr_edge_ref_lat(j)
          do k = kst+1, ked
            rprm_csph%edge_latitude(k)                                  &
     &         = rprm_csph%edge_ref_lat(j-1)                            &
     &          + (rprm_csph%edge_ref_lat(j)                            &
     &              - rprm_csph%edge_ref_lat(j-1))                      &
     &           * (rprm_csph%r_nod(k) - rprm_csph%r_nod(kst))          &
     &           / (rprm_csph%r_nod(ked) - rprm_csph%r_nod(kst))
          end do
        end do
!
        kst = rprm_csph%kr_edge_ref_lat(rprm_csph%nedge_ref_lat)
        do k = kst+1, rprm_csph%n_shell
          rprm_csph%edge_latitude(k)                                    &
     &      = rprm_csph%edge_ref_lat(rprm_csph%nedge_ref_lat)           &
     &     + (45.0d0 - rprm_csph%edge_ref_lat(rprm_csph%nedge_ref_lat)) &
     &      * (rprm_csph%r_nod(k) - rprm_csph%r_nod(kst))               &
     &       / (rprm_csph%r_nod(rprm_csph%n_shell)                      &
     &           - rprm_csph%r_nod(kst))
        end do
!
        write(*,*) 'edge_latitude', rprm_csph%n_shell
        do j = 1, rprm_csph%n_shell
          write(*,*) j, rprm_csph%edge_latitude(j)
        end do
      end if
!
      end subroutine set_cubed_rect_adjusting_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_shell_radius(rprm_csph)
!
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
!
      allocate(rprm_csph%r_nod(rprm_csph%n_shell))
      rprm_csph%r_nod = 0.0d0
!
      end subroutine alloc_shell_radius
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_edge_latitude(rprm_csph)
!
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
!
      allocate(rprm_csph%kr_edge_ref_lat(0:rprm_csph%nedge_ref_lat+1))
      allocate(rprm_csph%edge_ref_lat(rprm_csph%nedge_ref_lat))
      allocate(rprm_csph%edge_latitude(0:rprm_csph%n_shell))
!
      if(rprm_csph%nedge_ref_lat .gt. 0) then
        rprm_csph%kr_edge_ref_lat(1:rprm_csph%nedge_ref_lat) = 0
        rprm_csph%edge_ref_lat(1:rprm_csph%nedge_ref_lat) = 0.0d0
      end if
      rprm_csph%kr_edge_ref_lat(0) = 0
      rprm_csph%kr_edge_ref_lat(rprm_csph%nedge_ref_lat+1)              &
     &          = rprm_csph%n_shell
!
      rprm_csph%edge_latitude = 45.0d0
!
      end subroutine alloc_ref_edge_latitude
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_shell_radius(rprm_csph)
!
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
!
      deallocate(rprm_csph%r_nod)
!
      end subroutine dealloc_shell_radius
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_edge_latitude(rprm_csph)
!
      type(cubed_sph_radius), intent(inout) :: rprm_csph
!
!
      deallocate(rprm_csph%kr_edge_ref_lat)
      deallocate(rprm_csph%edge_ref_lat)
      deallocate(rprm_csph%edge_latitude)
!
      end subroutine dealloc_ref_edge_latitude
!
!  ---------------------------------------------------------------------
!
      end module t_cubed_sph_radius
