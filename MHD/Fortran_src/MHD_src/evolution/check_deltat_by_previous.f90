!check_deltat_by_previous.f90
!     module check_deltat_by_previous
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine s_check_deltat_by_previous
!
      module check_deltat_by_previous
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_flex_delta_t_data
!
      implicit  none
!
      private :: check_scalar_evo_by_previous
      private :: check_vector_evo_by_previous
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_check_deltat_by_previous
!
      use m_control_parameter
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      integer(kind = kint) :: ip, nd
!
!
!$omp parallel
      if(i_drmax_v .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_vector_evo_by_previous velo'
        call check_vector_evo_by_previous                               &
     &     (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,     &
     &      iphys%i_velo, iphys%i_chk_mom, iphys%i_chk_mom_2,           &
     &      i_drmax_v, nod_fld1%d_fld)
      end if
!
      if(i_drmax_p .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous press'
        call check_scalar_evo_by_previous                               &
     &     (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,     &
     &      iphys%i_press, iphys%i_chk_press, iphys%i_chk_press_2,      &
     &      i_drmax_p, nod_fld1%d_fld)
      end if
!
!
      if(i_drmax_b .gt. izero) then
        if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          if(iflag_debug .gt. izero)                                    &
     &      write(*,*) 'check_vector_evo_by_previous vecp'
          call check_vector_evo_by_previous                             &
     &       (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,   &
     &        iphys%i_vecp, iphys%i_chk_uxb, iphys%i_chk_uxb_2,         &
     &        i_drmax_b, nod_fld1%d_fld)
        else
          if(iflag_debug .gt. izero)                                    &
     &      write(*,*) 'check_vector_evo_by_previous magne'
          call check_vector_evo_by_previous                             &
     &       (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,   &
     &        iphys%i_magne, iphys%i_chk_uxb, iphys%i_chk_uxb_2,        &
     &        i_drmax_b, nod_fld1%d_fld)
        end if
      end if
!
      if(i_drmax_f .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous mag_p'
        call check_scalar_evo_by_previous                               &
     &     (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,     &
     &      iphys%i_mag_p, iphys%i_chk_potential,                       &
     &      iphys%i_chk_potential_2, i_drmax_f, nod_fld1%d_fld)
      end if
!
!
      if(i_drmax_t .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous temp'
        call check_scalar_evo_by_previous                               &
     &     (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,     &
     &      iphys%i_temp, iphys%i_chk_heat, iphys%i_chk_heat_2,         &
     &      i_drmax_t, nod_fld1%d_fld)
      end if
!
      if(i_drmax_d .gt. izero) then
        call check_scalar_evo_by_previous                               &
     &     (node1%numnod, node1%istack_nod_smp, nod_fld1%ntot_phys,     &
     &      iphys%i_light, iphys%i_chk_composit,                        &
     &      iphys%i_chk_composit_2, i_drmax_d, nod_fld1%d_fld)
      end if
!$omp end parallel
!
      d_ratio_max_l(1:ntot_dratio) = d_ratio_max_smp(1,1:ntot_dratio)
      d_ratio_min_l(1:ntot_dratio) = d_ratio_min_smp(1,1:ntot_dratio)
      do nd = 1, ntot_dratio
        do ip = 2, np_smp
          d_ratio_max_l(nd)                                             &
     &         = max(d_ratio_max_l(nd),d_ratio_max_smp(ip,nd))
          d_ratio_min_l(nd)                                             &
     &         = min(d_ratio_min_l(nd),d_ratio_min_smp(ip,nd))
        end do
      end do
!
      call MPI_allREDUCE (d_ratio_max_l, d_ratio_max, ntot_dratio,  &
     &    CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (d_ratio_min_l, d_ratio_min, ntot_dratio,  &
     &    CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
!
!
      d_ratio_allmax = d_ratio_max(1)
      d_ratio_allmin = d_ratio_min(1)
      do nd = 2, ntot_dratio
        d_ratio_allmax = max(d_ratio_allmax,d_ratio_max(nd))
        d_ratio_allmin = min(d_ratio_allmin,d_ratio_min(nd))
      end do
!
      end subroutine s_check_deltat_by_previous
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_scalar_evo_by_previous(numnod, inod_smp_stack,   &
     &          ncomp_nod, i_fld, i_chk, i_chk2, idrm, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_fld, i_chk, i_chk2, idrm
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: ip, ist, ied, inod
      real(kind = kreal) :: d_ratio
!
!
!$omp do private(ist,ied,inod,d_ratio)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!
        if( d_nod(ist,i_chk  ) .eq. d_nod(ist,i_chk2  )) then
          d_ratio_min_smp(ip,idrm  ) =  1.0d30
          d_ratio_max_smp(ip,idrm  ) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld  ) - d_nod(ist,i_chk2  ))      &
     &           / (two*(d_nod(ist,i_chk  ) - d_nod(ist,i_chk2  )) )
          d_ratio_max_smp(ip,idrm  ) =  abs(d_ratio - one)
          d_ratio_min_smp(ip,idrm  ) =  abs(d_ratio - one)
        end if
!
        do inod = ist+1, ied
          if( d_nod(inod,i_chk  ) .eq. d_nod(inod,i_chk2  )) then
            d_ratio_max_smp(ip,idrm)                                    &
     &            = max(d_ratio_max_smp(ip,idrm), zero)
            d_ratio_min_smp(ip,idrm)                                    &
     &            = min(d_ratio_min_smp(ip,idrm), 1.0d30)
          else
            d_ratio =    (d_nod(inod,i_fld  ) - d_nod(inod,i_chk2  ) )  &
     &            / (two*(d_nod(inod,i_chk  ) - d_nod(inod,i_chk2  ) ))
            d_ratio = abs(d_ratio - one)
!            write(100+my_rank,'(i16,1p4e16.5)') inod, d_ratio, d_nod(inod,i_fld  ), d_nod(inod,i_chk  ), d_nod(inod,i_chk2  )
            d_ratio_max_smp(ip,idrm)                                    &
     &            = max(d_ratio_max_smp(ip,idrm), d_ratio)
            d_ratio_min_smp(ip,idrm)                                    &
     &            = min(d_ratio_min_smp(ip,idrm), d_ratio)
          end if
        end do
      end do
!$omp end do nowait
!
      end subroutine check_scalar_evo_by_previous
!
! ----------------------------------------------------------------------
!
      subroutine check_vector_evo_by_previous(numnod, inod_smp_stack,   &
     &          ncomp_nod, i_fld, i_chk, i_chk2, idrm, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_fld, i_chk, i_chk2, idrm
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: ip, ist, ied, inod
      real(kind = kreal) :: d_ratio
!
!
!$omp do private(ist,ied,inod,d_ratio)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!
        if( d_nod(ist,i_chk  ) .eq. d_nod(ist,i_chk2  )) then
          d_ratio_min_smp(ip,idrm  ) =  1.0d30
          d_ratio_max_smp(ip,idrm  ) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld  ) - d_nod(ist,i_chk2  ))      &
     &           / (two*(d_nod(ist,i_chk  ) - d_nod(ist,i_chk2  )) )
          d_ratio_max_smp(ip,idrm  ) =  abs(d_ratio - one)
          d_ratio_min_smp(ip,idrm  ) =  abs(d_ratio - one)
        end if
!
        if( d_nod(ist,i_chk+1) .eq. d_nod(ist,i_chk2+1)) then
          d_ratio_min_smp(ip,idrm+1) =  1.0d30
          d_ratio_max_smp(ip,idrm+1) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld+1) - d_nod(ist,i_chk2+1))      &
     &           / (two*(d_nod(ist,i_chk+1) - d_nod(ist,i_chk2+1)) )
          d_ratio_max_smp(ip,idrm+1) =  abs(d_ratio - one)
          d_ratio_min_smp(ip,idrm+1) =  abs(d_ratio - one)
        end if
!
        if( d_nod(ist,i_chk+2) .eq. d_nod(ist,i_chk2+2)) then
          d_ratio_min_smp(ip,idrm+2) =  1.0d30
          d_ratio_max_smp(ip,idrm+2) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld+2) - d_nod(ist,i_chk2+2))      &
     &           / (two*(d_nod(ist,i_chk+2) - d_nod(ist,i_chk2+2)) )
          d_ratio_max_smp(ip,idrm+2) =  abs(d_ratio - one)
          d_ratio_min_smp(ip,idrm+2) =  abs(d_ratio - one)
        end if
!
        do inod = ist+1, ied
          if( d_nod(inod,i_chk  ) .eq. d_nod(inod,i_chk2  )) then
            d_ratio_max_smp(ip,idrm)                                    &
     &            = max(d_ratio_max_smp(ip,idrm), zero)
            d_ratio_min_smp(ip,idrm)                                    &
     &            = min(d_ratio_min_smp(ip,idrm), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld  ) - d_nod(inod,i_chk2  ))    &
     &           / (two*(d_nod(inod,i_chk  ) - d_nod(inod,i_chk2  )) )
            d_ratio = abs(d_ratio - one)
            d_ratio_max_smp(ip,idrm  )                                  &
     &            = max(d_ratio_max_smp(ip,idrm  ),d_ratio)
            d_ratio_min_smp(ip,idrm  )                                  &
     &            = min(d_ratio_min_smp(ip,idrm  ),d_ratio)
          end if
!
          if( d_nod(inod,i_chk+1) .eq. d_nod(inod,i_chk2+1)) then
            d_ratio_max_smp(ip,idrm+1)                                  &
     &            = max(d_ratio_max_smp(ip,idrm+1), zero)
            d_ratio_min_smp(ip,idrm+1)                                  &
     &            = min(d_ratio_min_smp(ip,idrm+1), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld+1) - d_nod(inod,i_chk2+1))    &
     &           / (two*(d_nod(inod,i_chk+1) - d_nod(inod,i_chk2+1)) )
            d_ratio = abs(d_ratio - one)
            d_ratio_max_smp(ip,idrm+1)                                  &
     &            = max(d_ratio_max_smp(ip,idrm+1),d_ratio)
            d_ratio_min_smp(ip,idrm+1)                                  &
     &            = min(d_ratio_min_smp(ip,idrm+1),d_ratio)
          end if
!
          if( d_nod(inod,i_chk+2) .eq. d_nod(inod,i_chk2+2)) then
            d_ratio_max_smp(ip,idrm+2)                                  &
     &            = max(d_ratio_max_smp(ip,idrm+2), zero)
            d_ratio_min_smp(ip,idrm+2)                                  &
     &            = min(d_ratio_min_smp(ip,idrm+2), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld+2) - d_nod(inod,i_chk2+2))    &
     &           / (two*(d_nod(inod,i_chk+2) - d_nod(inod,i_chk2+2)) )
            d_ratio = abs(d_ratio - one)
            d_ratio_max_smp(ip,idrm+2)                                  &
     &            = max(d_ratio_max_smp(ip,idrm+2),d_ratio)
            d_ratio_min_smp(ip,idrm+2)                                  &
     &            = max(d_ratio_min_smp(ip,idrm+2),d_ratio)
          end if
        end do
      end do
!$omp end do nowait
!
      end subroutine check_vector_evo_by_previous
!
! ----------------------------------------------------------------------
!
      end module  check_deltat_by_previous
