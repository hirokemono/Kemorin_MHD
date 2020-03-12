!cal_helicities.f90
!      module cal_helicities
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!
!!@verbatim
!!      subroutine cal_helicity(iphys, nod_fld)
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module cal_helicities
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_helicity(iphys, nod_fld)
!
      use products_nodal_fields_smp
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      if (iphys%prod_fld%i_k_heli .gt. izero) then
         call cal_phys_dot_product                                      &
     &      (iphys%i_velo, iphys%base%i_vort, iphys%prod_fld%i_k_heli,  &
     &       nod_fld)
      end if
!
      if (iphys%prod_fld%i_m_heli .gt. izero) then
         call cal_phys_dot_product                                      &
     &      (iphys%base%i_vecp, iphys%base%i_magne,                     &
     &       iphys%prod_fld%i_m_heli, nod_fld)
      end if
!
      if (iphys%prod_fld%i_c_heli .gt. izero) then
         call cal_phys_dot_product                                      &
     &      (iphys%base%i_magne, iphys%base%i_current,                  &
     &       iphys%prod_fld%i_c_heli, nod_fld)
      end if
!
      if (iphys%prod_fld%i_x_heli .gt. izero) then
         call cal_phys_dot_product                                      &
     &      (iphys%i_velo, iphys%base%i_magne, iphys%prod_fld%i_x_heli, &
     &       nod_fld)
      end if
!$omp end parallel
!
      end subroutine cal_helicity
!
! ----------------------------------------------------------------------
!
      end module cal_helicities
