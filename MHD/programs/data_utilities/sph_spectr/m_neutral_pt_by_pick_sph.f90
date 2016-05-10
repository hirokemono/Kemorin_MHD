!>@file   m_neutral_pt_by_pick_sph.f90
!!@brief      module m_neutral_pt_by_pick_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief choose spectr data to output
!!
!!@verbatim
!!      subroutine alloc_neutral_point
!!      subroutine dealloc_neutral_point
!!      subroutine find_field_address
!!      subroutine set_radius_for_fdm
!!      subroutine set_radial_grad_scalars(istep, time, buo_ratio)
!!@endverbatim
!
      module m_neutral_pt_by_pick_sph
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use m_phys_labels
!
      use m_pickup_sph_spectr_data
      use m_fdm_coefs
      use set_radius_func
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
      real(kind = kreal), allocatable :: temp00(:)
      real(kind = kreal), allocatable :: comp00(:)
      real(kind = kreal), allocatable :: grad_temp00(:)
      real(kind = kreal), allocatable :: grad_comp00(:)
      real(kind = kreal), allocatable :: freq(:)
      real(kind = kreal), allocatable :: freq2(:)
!
      integer(kind = kint) :: icomp_temp, icomp_light, ipick_l0m0
!
      private :: id_neutral_pt, fname_neutral_pt
      private :: id_ave_den, fname_ave_den
      private :: temp00, comp00, grad_temp00, grad_comp00, freq, freq2
      private :: icomp_temp, icomp_light, ipick_l0m0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_neutral_point
!
      allocate(temp00(num_pick_layer))
      allocate(comp00(num_pick_layer))
      allocate(grad_temp00(num_pick_layer))
      allocate(grad_comp00(num_pick_layer))
      allocate(freq(num_pick_layer))
      allocate(freq2(num_pick_layer))
      temp00 = 0.0d0
      comp00 = 0.0d0
      grad_temp00 = 0.0d0
      grad_comp00 = 0.0d0
      freq =  0.0d0
      freq2 = 0.0d0
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
      subroutine dealloc_neutral_point
!
      deallocate(temp00, grad_temp00)
      deallocate(comp00, grad_comp00, freq, freq2)
!
      close(id_neutral_pt)
      close(id_ave_den)
!
      end subroutine dealloc_neutral_point
!
! ----------------------------------------------------------------------
!
      subroutine find_field_address
!
      integer(kind = kint) :: i
!
      do i = 1, ntot_comp_pick_sph
        if(pick_sph_spec_name(i) .eq. fhd_temp)  icomp_temp =  i
        if(pick_sph_spec_name(i) .eq. fhd_light) icomp_light = i
      end do
!
      end subroutine find_field_address
!
! ----------------------------------------------------------------------
!
      subroutine set_radius_for_fdm
!
      use t_spheric_rj_data
!
      integer(kind = kint) :: i
!
!
      sph_rj1%nidx_rj(1) = num_pick_layer
      sph_rj1%nidx_rj(2) = 1
      nidx_rj(1:2) = sph_rj1%nidx_rj(1:2)
      call alloc_type_sph_1d_index_rj(sph_rj1)
!
      do i = 1, num_pick_layer
        sph_rj1%radius_1d_rj_r(i) = r_pick_layer(i)
      end do
      do i = 1, num_pick_sph_mode
        if(idx_pick_sph_gl(i,1) .eq. 0) ipick_l0m0 = i
      end do
!
      call allocate_dr_rj_noequi(sph_rj1%nidx_rj(1))
      call set_dr_for_nonequi(sph_param1%nlayer_CMB,                    &
     &   sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r)
      call const_2nd_fdm_matrices
      call const_2nd_fdm_coefs
!
      write(*,*) 'icomp_temp, icomp_light', icomp_temp, icomp_light
      write(*,*) 'ipick_l0m0', ipick_l0m0
!
      end subroutine set_radius_for_fdm
!
! ----------------------------------------------------------------------
!
      subroutine set_radial_grad_scalars(istep, time,                   &
     &          nri, radius_1d_rj_r, buo_ratio)
!
      use m_sph_phys_address
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time, buo_ratio
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint) :: k, ipick
!
      real(kind = kreal) :: r_neut
!
      do k = 1, num_pick_layer
        ipick = k + (ipick_l0m0-1) * num_pick_layer
        temp00(k) = d_rj_pick_sph_gl(icomp_temp, ipick)
        comp00(k) = d_rj_pick_sph_gl(icomp_light,ipick)
      end do
!
      do k = 2, num_pick_layer-1
        grad_temp00(k) =  d1nod_mat_fdm_2(k,-1) * temp00(k-1)           &
     &                  + d1nod_mat_fdm_2(k, 0) * temp00(k  )           &
     &                  + d1nod_mat_fdm_2(k, 1) * temp00(k+1)
        grad_comp00(k) =  d1nod_mat_fdm_2(k,-1) * comp00(k-1)           &
     &                  + d1nod_mat_fdm_2(k, 0) * comp00(k  )           &
     &                  + d1nod_mat_fdm_2(k, 1) * comp00(k+1)
!
        freq2(k) = buo_ratio * grad_comp00(k) + grad_temp00(k)
        if(freq2(k) .gt. 0.0d0) freq(k) = sqrt(freq2(k))
        freq2(k) = freq2(k) * radius_1d_rj_r(k  )**2
      end do
!
      do k = num_pick_layer-2, 2, - 1
        if( freq2(k).lt.0.0d0 .and. freq2(k+1).ge.0.0d0) then
          r_neut = (radius_1d_rj_r(k  )*abs(freq2(k+1))                 &
     &            + radius_1d_rj_r(k+1)*abs(freq2(k)  ) )               &
     &             / (abs(freq2(k+1) - freq2(k)))
          write(id_neutral_pt,'(i15,1p2E25.15e3)') istep, time, r_neut
        end if
      end do
!
      do k = 1, num_pick_layer
            write(id_ave_den,'(i15,1pE25.15e3,i15,1p7E25.15e3)')        &
     &           istep, time, k, radius_1d_rj_r(k),                     &
     &           temp00(k), comp00(k), grad_temp00(k), grad_comp00(k),  &
     &           freq2(k), freq(k)
      end do
!
       end subroutine set_radial_grad_scalars
!
! ----------------------------------------------------------------------
!
      end module m_neutral_pt_by_pick_sph
