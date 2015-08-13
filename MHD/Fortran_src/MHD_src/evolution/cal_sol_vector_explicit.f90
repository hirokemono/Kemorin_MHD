!
!      module cal_sol_vector_explicit
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_pre_euler
!      subroutine cal_sol_temp_euler
!      subroutine cal_sol_part_temp_euler
!      subroutine cal_sol_vect_p_pre_euler
!      subroutine cal_sol_magne_pre_euler
!      subroutine cal_sol_d_scalar_euler
!
!      subroutine cal_sol_velo_pre_adams
!      subroutine cal_sol_temp_adams
!      subroutine cal_sol_part_temp_adams
!      subroutine cal_sol_vect_p_pre_adams
!      subroutine cal_sol_magne_pre_adams
!      subroutine cal_sol_d_scalar_adams
!
      module cal_sol_vector_explicit
!
      use m_precision
!
      implicit none
!
      private :: cal_sol_vect_pre_fluid_euler
      private :: cal_sol_vect_pre_conduct_euler
      private :: cal_sol_vect_pre_fluid_adams
      private :: cal_sol_vect_pre_conduct_adams
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_euler
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_euler(node1%istack_internal_smp,      &
     &    n_vector, iphys%i_velo)
!
      end subroutine cal_sol_velo_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_euler
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_euler(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_temp)
!
      end subroutine cal_sol_temp_euler
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_part_temp_euler
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_euler(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_par_temp)
!
      end subroutine cal_sol_part_temp_euler
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_vect_p_pre_euler
!
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_conduct_euler(n_vector, iphys%i_vecp)
!
      end subroutine cal_sol_vect_p_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_euler
!
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_conduct_euler(n_vector, iphys%i_magne)
!
      end subroutine cal_sol_magne_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_euler
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_euler(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_light)
!
      end subroutine cal_sol_d_scalar_euler
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_adams
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_adams(node1%istack_internal_smp,      &
     &    n_vector, iphys%i_velo,iphys%i_pre_mom)
!
      end subroutine cal_sol_velo_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_adams
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_adams(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_temp, iphys%i_pre_heat)
!
      end subroutine cal_sol_temp_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_part_temp_adams
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_adams(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_par_temp, iphys%i_pre_heat)
!
      end subroutine cal_sol_part_temp_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_adams
!
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_conduct_adams(n_vector, iphys%i_vecp,       &
     &    iphys%i_pre_uxb)
!
      end subroutine cal_sol_vect_p_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_adams
!
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_conduct_adams(n_vector, iphys%i_magne,      &
     &    iphys%i_pre_uxb)
!
      end subroutine cal_sol_magne_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_adams
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
!
!
      call cal_sol_vect_pre_fluid_adams(node1%istack_internal_smp,      &
     &    n_scalar, iphys%i_light, iphys%i_pre_composit)
!
      end subroutine cal_sol_d_scalar_adams
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_fluid_euler                           &
     &         (inter_smp_stack, numdir, i_field)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numdir, i_field
!
      integer (kind = kint) :: iproc, inod, nd, icomp, ist, ied
!
!
!$omp parallel do private(nd,icomp,inod,ist,ied)
      do iproc = 1, np_smp
        do nd=1, numdir
          icomp = i_field + nd - 1
          ist = inter_smp_stack(iproc-1)+1
          ied = inter_smp_stack(iproc)
          do inod = ist, ied
             d_nod(inod,icomp) = d_nod(inod,icomp)                      &
     &            + ml_fl(inod)*(ff(inod,nd) + ff_nl(inod,nd) )*dt
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_fluid_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_conduct_euler(numdir, i_field)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: numdir, i_field
!
      integer (kind = kint) :: iproc, inum, inod, nd, icomp
      integer (kind = kint) :: istart, iend
!
!
!$omp parallel do private(nd,icomp,inum,inod)
      do iproc = 1, np_smp
        istart = inter_cd_smp_stack(iproc-1) + 1
        iend = inter_cd_smp_stack(iproc)
        do nd=1, numdir
          icomp = i_field + nd - 1
!cdir nodep
          do inum = istart, iend
            inod = inod_conduct(inum)
!
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &           + ml_cd(inod) * ( ff(inod,nd) + ff_nl(inod,nd) ) * dt
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_conduct_euler
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_fluid_adams                           &
     &         (inter_smp_stack, numdir, i_field, if_pre)
!
      use m_machine_parameter
      use m_node_phys_data
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
!
      integer (kind = kint) :: iproc, inod, nd, icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,icomp,if_comp,inod,ist,ied)
      do iproc = 1, np_smp
        do nd=1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
          ist = inter_smp_stack(iproc-1)+1
          ied = inter_smp_stack(iproc)
          do inod = ist, ied
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &       + ml_fl(inod)*adam_0 * ( ff(inod,nd)+ff_nl(inod,nd) ) * dt &
     &       + ml_fl(inod)*adam_1 * d_nod(inod,if_comp) * dt
            d_nod(inod,if_comp) = ff(inod,nd) + ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_fluid_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_conduct_adams(numdir, i_field,        &
     &          if_pre)
!
      use m_machine_parameter
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
!
      integer (kind = kint) :: nd, icomp, if_comp
      integer (kind = kint) :: istart, iend
      integer (kind = kint) :: iproc, inod, inum
!
!
!$omp parallel do private(nd,istart,iend,icomp,if_comp,inum,inod)
      do iproc = 1, np_smp
        istart = inter_cd_smp_stack(iproc-1) + 1
        iend = inter_cd_smp_stack(iproc)
        do nd = 1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inum = istart, iend
            inod = inod_conduct(inum)
!
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &       + adam_0 * ml_cd(inod) * (ff(inod,nd)+ff_nl(inod,nd)) * dt &
     &       + adam_1 * ml_cd(inod) * d_nod(inod,if_comp) * dt
            d_nod(inod,if_comp) = ff(inod,nd) + ff_nl(inod,nd)
!
        end do
       end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_conduct_adams
!
! -----------------------------------------------------------------------!
      end module cal_sol_vector_explicit
