!
!      module cal_sol_vector_pre_crank
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_pre_linear
!      subroutine cal_sol_temp_linear
!      subroutine cal_sol_par_temp_linear
!      subroutine cal_sol_vect_p_pre_linear
!      subroutine cal_sol_magne_pre_linear
!      subroutine cal_sol_d_scalar_linear
!
      module cal_sol_vector_pre_crank
!
      use m_precision
!
      implicit none
!
      private :: cal_sol_vec_fluid_linear, cal_sol_vec_conduct_linear
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_linear
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo,                   &
     &    iphys%i_pre_mom, d_nod)
!
      end subroutine cal_sol_velo_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_linear
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_temp,                   &
     &    iphys%i_pre_heat, d_nod)
!
      end subroutine cal_sol_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_par_temp_linear
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_par_temp,               &
     &    iphys%i_pre_heat, d_nod)
!
      end subroutine cal_sol_par_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_linear
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_conduct_linear                                   &
     &   (node1%numnod, node1%istack_internal_smp, inter_cd_smp_stack,  &
     &    numnod_conduct, inod_conduct, nod_fld1%ntot_phys, n_vector,   &
     &    iphys%i_vecp, iphys%i_pre_uxb, d_nod)
!
      end subroutine cal_sol_vect_p_pre_linear
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_magne_pre_linear
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_conduct_linear                                   &
     &   (node1%numnod, node1%istack_internal_smp, inter_cd_smp_stack,  &
     &    numnod_conduct, inod_conduct, nod_fld1%ntot_phys, n_vector,   &
     &    iphys%i_magne, iphys%i_pre_uxb, d_nod)
!
      end subroutine cal_sol_magne_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_linear
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_light,                  &
     &    iphys%i_pre_composit, d_nod)
!
      end subroutine cal_sol_d_scalar_linear
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_fluid_linear(numnod, inod_smp_stack,       &
     &          ncomp_nod, numdir, i_field, if_pre, d_nod)
!
      use m_machine_parameter
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      integer (kind = kint) :: iproc, inod, nd
      integer (kind = kint) :: icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,icomp,if_comp,inod)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
        do nd = 1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inod = inod_smp_stack(iproc-1)+1, inod_smp_stack(iproc)
            ff(inod,nd) = d_nod(inod,icomp) * ml_o_fl(inod)             &
     &                   + ( ff(inod,nd)                                &
     &                   + adam_0*ff_nl(inod,nd)                        &
     &                   + adam_1* d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_sol_vec_fluid_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_conduct_linear(numnod, inter_smp_stack,    &
     &          inter_cd_smp_stack, nnod_cd, inod_conduct,              &
     &          ncomp_nod, numdir, i_field, if_pre, d_nod)
!
      use m_machine_parameter
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: numnod, nnod_cd, ncomp_nod
      integer (kind = kint), intent(in) :: inod_conduct(nnod_cd)
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inter_cd_smp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inum, inod, nd
      integer (kind = kint) :: icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,ist,ied,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = ff(inod,nd) * dt
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(nd,ist,ied,inum,inod,icomp,if_comp)
      do iproc = 1, np_smp
        ist = inter_cd_smp_stack(iproc-1) + 1
        ied = inter_cd_smp_stack(iproc)
        do nd=1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inum = ist, ied
            inod = inod_conduct(inum)
!
            ff(inod,nd) = d_nod(inod,icomp) * ml_o_cd(inod)             &
     &                   + ff(inod,nd)                                  &
     &                   + ( adam_0 * ff_nl(inod,nd)                    &
     &                    + adam_1 * d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd) 
!
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_sol_vec_conduct_linear
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_pre_crank
