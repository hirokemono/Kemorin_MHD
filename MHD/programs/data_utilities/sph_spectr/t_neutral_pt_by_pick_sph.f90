!>@file   t_neutral_pt_by_pick_sph.f90
!!@brief      module t_neutral_pt_by_pick_sph
!!
!!@author H. Matsui
!!@date Programmed in  Dec., 2012
!
!> @brief choose spectr data to output
!!
!!@verbatim
!!      subroutine alloc_neutral_point(num_layer, ntl)
!!      subroutine dealloc_neutral_point(ntl)
!!      subroutine find_field_address(pick_IO, ntl)
!!      subroutine set_radius_for_fdm                                   &
!!     &         (pick_IO, sph_params, sph_rj, r_2nd, ntl)
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rj_grid), intent(inout) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: r_2nd
!!      subroutine set_radial_grad_scalars(istep, time,                 &
!!     &          nri, radius_1d_rj_r, d1nod_mat_fdm_2, buo_ratio,      &
!!     &          picked, ntl1)
!!@endverbatim
!
      module t_neutral_pt_by_pick_sph
!
      use m_precision
      use m_constants
      use m_phys_labels
!
      use t_picked_sph_spectr_data_IO
!
      use set_radius_func_noequi
!
      implicit  none
!
      integer(kind = kint), parameter :: id_neutral_pt = 98
      integer(kind = kint), parameter :: id_ave_den = 99
      character(len=kchara), parameter                                  &
     &               :: fname_neutral_pt = "neutral_point.dat"
      character(len=kchara), parameter                                  &
     &               :: fname_ave_den = "ave_density.dat"
!
      type neutral_pt_by_pick_sph
        real(kind = kreal), allocatable :: temp00(:)
        real(kind = kreal), allocatable :: comp00(:)
        real(kind = kreal), allocatable :: grad_temp00(:)
        real(kind = kreal), allocatable :: grad_comp00(:)
        real(kind = kreal), allocatable :: freq(:)
        real(kind = kreal), allocatable :: freq2(:)
!
        integer(kind = kint) :: ist_r = 1
        integer(kind = kint) :: icomp_temp = 0
        integer(kind = kint) :: icomp_light = 0
        integer(kind = kint), allocatable :: ipick_l0m0(:)
      end type neutral_pt_by_pick_sph
!
      private :: id_neutral_pt, fname_neutral_pt
      private :: id_ave_den, fname_ave_den
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_neutral_point(num_layer, ntl)
!
      integer(kind = kint), intent(in) :: num_layer
!
      type(neutral_pt_by_pick_sph), intent(inout) :: ntl
!
      allocate(ntl%ipick_l0m0(0:num_layer))
!
      allocate(ntl%temp00(0:num_layer))
      allocate(ntl%comp00(0:num_layer))
      allocate(ntl%grad_temp00(0:num_layer))
      allocate(ntl%grad_comp00(0:num_layer))
      allocate(ntl%freq(0:num_layer))
      allocate(ntl%freq2(0:num_layer))
!
      ntl%ipick_l0m0 = -1
!
      ntl%temp00 = 0.0d0
      ntl%comp00 = 0.0d0
      ntl%grad_temp00 = 0.0d0
      ntl%grad_comp00 = 0.0d0
      ntl%freq =  0.0d0
      ntl%freq2 = 0.0d0
!
      open(id_neutral_pt, file=fname_neutral_pt)
      open(id_ave_den, file=fname_ave_den)
!
      write(id_neutral_pt,'(a)') 'time_step  time  neutral_radius'
      write(id_ave_den,'(3a)') 'time_step  time  radius_ID  radius  ',  &
     &                      '  temperature  composition  heat_flux',    &
     &                      '   comp_flux  density_flux  freq'
!
      end subroutine alloc_neutral_point
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_neutral_point(ntl)
!
      type(neutral_pt_by_pick_sph), intent(inout) :: ntl
!
!
      deallocate(ntl%temp00, ntl%grad_temp00)
      deallocate(ntl%comp00, ntl%grad_comp00, ntl%freq, ntl%freq2)
!
      close(id_neutral_pt)
      close(id_ave_den)
!
      end subroutine dealloc_neutral_point
!
! ----------------------------------------------------------------------
!
      subroutine find_field_address(pick_IO, ntl)
!
      type(picked_spectrum_data_IO), intent(in) :: pick_IO
      type(neutral_pt_by_pick_sph), intent(inout) :: ntl
!
!
      integer(kind = kint) :: i
!
      do i = 1, pick_IO%ntot_comp
        if(pick_IO%spectr_name(i) .eq. fhd_temp) then
          ntl%icomp_temp =  i
          exit
        end if
      end do
!
      do i = 1, pick_IO%ntot_comp
        if(pick_IO%spectr_name(i) .eq. fhd_light) then
          ntl%icomp_light =  i
          exit
        end if
      end do
!
      end subroutine find_field_address
!
! ----------------------------------------------------------------------
!
      subroutine set_radius_for_fdm                                     &
     &         (pick_IO, sph_params, sph_rj, r_2nd, ntl)
!
      use t_spheric_parameter
      use t_fdm_coefs
      use const_fdm_coefs
!
      type(picked_spectrum_data_IO), intent(in) :: pick_IO
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rj_grid), intent(inout) ::  sph_rj
      type(fdm_matrices), intent(inout) :: r_2nd
!
      type(neutral_pt_by_pick_sph), intent(inout) :: ntl
!
      integer(kind = kint) :: i, k
      integer(kind = kint) :: minmax(4)
!
!
      minmax = maxval(pick_IO%idx_sph,1)
      ntl%ist_r = minmax(1)
!
      sph_rj%nidx_rj(1) = pick_IO%num_layer
      sph_rj%nidx_rj(2) = 1
      call alloc_type_sph_1d_index_rj(sph_rj)
      call alloc_neutral_point(pick_IO%num_layer, ntl)
!
      do i = 1, pick_IO%ntot_pick_spectr
        k = pick_IO%idx_sph(i,1)
        sph_rj%radius_1d_rj_r(k) = pick_IO%radius(i)
        if(pick_IO%idx_sph(i,2) .eq. 0) then
          ntl%ipick_l0m0(k) = i
        end if
      end do
!
      call allocate_dr_rj_noequi(sph_rj%nidx_rj(1))
      call set_dr_for_nonequi(sph_params%nlayer_CMB,                    &
     &   sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
      call const_2nd_fdm_matrices(sph_params, sph_rj, r_2nd)
!
      write(*,*) 'icomp_temp, icomp_light',                             &
     &           ntl%icomp_temp, ntl%icomp_light
      write(*,*) 'ipick_l0m0', ntl%ipick_l0m0
!
      end subroutine set_radius_for_fdm
!
! ----------------------------------------------------------------------
!
      subroutine set_radial_grad_scalars(istep, time,                   &
     &          nri, radius_1d_rj_r, d1nod_mat_fdm_2, buo_ratio,        &
     &          pick_IO, ntl)
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time, buo_ratio
      type(picked_spectrum_data_IO), intent(in) :: pick_IO
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!
      type(neutral_pt_by_pick_sph), intent(inout) :: ntl
!
      integer(kind = kint) :: k, ipick
!
      real(kind = kreal) :: r_neut
!
!
      if(ntl%ipick_l0m0(0) .gt. 0) then
        ipick = ntl%ipick_l0m0(0)
        ntl%temp00(0) = pick_IO%d_pk(ntl%icomp_temp, ipick)
        ntl%comp00(0) = pick_IO%d_pk(ntl%icomp_light,ipick)
      end if
!
      do k = 1, pick_IO%num_layer
        ipick = ntl%ipick_l0m0(k)
        ntl%temp00(k) = pick_IO%d_pk(ntl%icomp_temp, ipick)
        ntl%comp00(k) = pick_IO%d_pk(ntl%icomp_light,ipick)
      end do
!
!
      if(ntl%ipick_l0m0(0) .gt. 0) then
        ipick = ntl%ipick_l0m0(0)
        ntl%grad_temp00(0) = 0.0d0
        ntl%grad_comp00(0) = 0.0d0
!
        ntl%grad_temp00(1) =  d1nod_mat_fdm_2(1,-1) * ntl%temp00(0)     &
     &                      + d1nod_mat_fdm_2(1, 0) * ntl%temp00(1)     &
     &                      + d1nod_mat_fdm_2(1, 1) * ntl%temp00(2)
        ntl%grad_comp00(1) =  d1nod_mat_fdm_2(1,-1) * ntl%comp00(0)     &
     &                      + d1nod_mat_fdm_2(1, 0) * ntl%comp00(1)     &
     &                      + d1nod_mat_fdm_2(1, 1) * ntl%comp00(2)
      else
        ntl%grad_temp00(1) =  d1nod_mat_fdm_2(1, 0) * ntl%temp00(1)     &
     &                      + d1nod_mat_fdm_2(1, 1) * ntl%temp00(2)
        ntl%grad_comp00(1) =  d1nod_mat_fdm_2(1, 0) * ntl%comp00(1)     &
     &                      + d1nod_mat_fdm_2(1, 1) * ntl%comp00(2)
      end if
!
      do k = 2, pick_IO%num_layer - 1
        ntl%grad_temp00(k) =  d1nod_mat_fdm_2(k,-1) * ntl%temp00(k-1)   &
     &                      + d1nod_mat_fdm_2(k, 0) * ntl%temp00(k  )   &
     &                      + d1nod_mat_fdm_2(k, 1) * ntl%temp00(k+1)
        ntl%grad_comp00(k) =  d1nod_mat_fdm_2(k,-1) * ntl%comp00(k-1)   &
     &                      + d1nod_mat_fdm_2(k, 0) * ntl%comp00(k  )   &
     &                      + d1nod_mat_fdm_2(k, 1) * ntl%comp00(k+1)
      end do
!
      k = pick_IO%num_layer
        ntl%grad_temp00(k) =  d1nod_mat_fdm_2(k,-1) * ntl%temp00(k-1)   &
     &                      + d1nod_mat_fdm_2(k, 0) * ntl%temp00(k  )
        ntl%grad_comp00(k) =  d1nod_mat_fdm_2(k,-1) * ntl%comp00(k-1)   &
     &                      + d1nod_mat_fdm_2(k, 0) * ntl%comp00(k  )
!
      do k = 1, pick_IO%num_layer
        ntl%freq2(k) = buo_ratio * ntl%grad_comp00(k)                   &
     &                           + ntl%grad_temp00(k)
        if(ntl%freq2(k) .gt. 0.0d0) ntl%freq(k) = sqrt(ntl%freq2(k))
        ntl%freq2(k) = ntl%freq2(k) * radius_1d_rj_r(k  )**2
      end do
!
      do k = pick_IO%num_layer - 2, 2, - 1
        if(ntl%freq2(k).lt.0.0d0 .and. ntl%freq2(k+1).ge.0.0d0) then
          r_neut = (radius_1d_rj_r(k  )*abs(ntl%freq2(k+1))             &
     &            + radius_1d_rj_r(k+1)*abs(ntl%freq2(k)  ) )           &
     &             / (abs(ntl%freq2(k+1) - ntl%freq2(k)))
          write(id_neutral_pt,'(i15,1p2E25.15e3)') istep, time, r_neut
        end if
      end do
!
      do k = 1, pick_IO%num_layer
        write(id_ave_den,'(i15,1pE25.15e3,i15,1p7E25.15e3)')            &
     &           istep, time, k, radius_1d_rj_r(k),                     &
     &           ntl%temp00(k), ntl%comp00(k),                          &
     &           ntl%grad_temp00(k), ntl%grad_comp00(k),                &
     &           ntl%freq2(k), ntl%freq(k)
      end do
!
       end subroutine set_radial_grad_scalars
!
! ----------------------------------------------------------------------
!
      end module t_neutral_pt_by_pick_sph
