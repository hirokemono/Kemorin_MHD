!
!      module cal_line_filtering_tensor
!
!     Written by H. Matsui in 2004
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine cal_l_filtering_tensor(i_filter)
!        filtering along each directions for vector field
!          i_filter(input) :: node data ID for original field
!          i_filter(output) :: node data ID for filtered field
!
      module cal_line_filtering_tensor
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_l_filtering_tensor(i_filter)
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_node_phys_data
      use m_l_filtering_data
      use m_l_filtering_data_smp
!
      integer (kind = kint), intent(in) :: i_filter
!
      integer (kind = kint) :: ip, inod, jnod, nd
      integer (kind = kint) :: ist, ied, idx, isum, i, ii
!
!
      do nd = 1, 3
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
        do inod = ist, ied
          ff_lf_smp(inod,1) = 0.0d0
          ff_lf_smp(inod,2) = 0.0d0
          ff_lf_smp(inod,3) = 0.0d0
          ff_lf_smp(inod,4) = 0.0d0
          ff_lf_smp(inod,5) = 0.0d0
          ff_lf_smp(inod,6) = 0.0d0
        end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,i,idx,isum,ii)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        do isum = 1, nmax_l_filter(nd)
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
         do i = 1, num_4_lf_smp(isum,ip,nd)
           ii = (ip-1)*nsize_lf_smp + isum
           inod = inod_smp_stack(ip-1) + i
           idx = istack_l_filter_smp(ii-1,nd) + i
           jnod = item_l_filter_smp(idx,nd)
           ff_lf_smp(inod,1) = ff_lf_smp(inod,1)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter  )
           ff_lf_smp(inod,2) = ff_lf_smp(inod,2)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+1)
           ff_lf_smp(inod,3) = ff_lf_smp(inod,3)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+2)
           ff_lf_smp(inod,4) = ff_lf_smp(inod,4)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+3)
           ff_lf_smp(inod,5) = ff_lf_smp(inod,5)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+4)
           ff_lf_smp(inod,6) = ff_lf_smp(inod,6)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+5)
          end do
        end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
        do jnod = ist, ied
          inod = inod_l_filter_smp(jnod,nd)
          d_nod(inod,i_filter  ) = ff_lf_smp(jnod,1)
          d_nod(inod,i_filter+1) = ff_lf_smp(jnod,2)
          d_nod(inod,i_filter+2) = ff_lf_smp(jnod,3)
          d_nod(inod,i_filter+3) = ff_lf_smp(jnod,4)
          d_nod(inod,i_filter+4) = ff_lf_smp(jnod,5)
          d_nod(inod,i_filter+5) = ff_lf_smp(jnod,6)
        end do
       end do
!$omp end parallel do
!
      end do
!
      end subroutine cal_l_filtering_tensor
!
!-----------------------------------------------------------------------
!
      end module cal_line_filtering_tensor
