!cal_sgs_fluxes_diffuse.f90
!      module cal_sgs_fluxes_diffuse
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_h_flux_diffuse
!!      subroutine cal_sgs_m_flux_diffuse(numnod,                       &
!!     &          ncomp_nod, i_vect, i_sgs_diffuse, i_sgs, d_nod)
!
      module cal_sgs_fluxes_diffuse
!
      use m_precision
!
      use m_constants
      use cal_gradient_w_const
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_h_flux_diffuse
!
      use m_control_parameter
      use m_node_phys_data
!
!
      call cal_gradent_in_fluid_w_const(iflag_temp_supg,               &
     &    iphys%i_SGS_h_flux, iphys%i_sgs_temp, dminus)
!
      end subroutine cal_sgs_h_flux_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_diffuse(numnod,                         &
     &          ncomp_nod, i_vect, i_sgs_diffuse, i_sgs, d_nod)
!
      use m_control_parameter
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_sgs_diffuse
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind=kint) :: inod
!
!
      call cal_gradent_in_fluid_w_const(iflag_velo_supg,                &
     &    i_sgs, i_vect, dminus)
      call cal_gradent_in_fluid_w_const(iflag_velo_supg,                &
     &   i_sgs_diffuse, (i_vect+1), dminus)
      call cal_gradent_in_fluid_w_const(iflag_velo_supg,                &
     &   i_sgs_diffuse+3, (i_vect+2), dminus)
!
!
!$omp parallel do
      do inod = 1, numnod
        d_nod(inod,i_sgs  ) = two * d_nod(inod,i_sgs  )
        d_nod(inod,i_sgs+1) =       d_nod(inod,i_sgs+1)                 &
     &                            + d_nod(inod,i_sgs_diffuse  )
        d_nod(inod,i_sgs+2) =       d_nod(inod,i_sgs+2)                 &
     &                            + d_nod(inod,i_sgs_diffuse+3)
        d_nod(inod,i_sgs+3) = two * d_nod(inod,i_sgs_diffuse+1)
        d_nod(inod,i_sgs+4) =       d_nod(inod,i_sgs_diffuse+2)         &
     &                            + d_nod(inod,i_sgs_diffuse+4)
        d_nod(inod,i_sgs+5) = two * d_nod(inod,i_sgs_diffuse+5)
      end do
!$omp end parallel do
!
      end subroutine cal_sgs_m_flux_diffuse
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes_diffuse
