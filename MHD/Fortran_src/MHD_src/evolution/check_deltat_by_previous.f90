!check_deltat_by_previous.f90
!     module check_deltat_by_previous
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine s_check_deltat_by_previous                           &
!!   &           (node, cd_prop, iphys, nod_fld, flex_data)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(flexible_stepping_data), intent(inout) :: flex_data
!
      module check_deltat_by_previous
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_flex_delta_t_data
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
      subroutine s_check_deltat_by_previous                             &
     &         (node, cd_prop, iphys, nod_fld, flex_data)
!
      type(conductive_property), intent(in) :: cd_prop
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(flexible_stepping_data), intent(inout) :: flex_data
!
      integer(kind = kint) :: ip, nd
!
!
!$omp parallel
      if(flex_data%i_drmax_v .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_vector_evo_by_previous velo'
        call check_vector_evo_by_previous                               &
     &     (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,        &
     &      iphys%i_velo, iphys%i_chk_mom, iphys%i_chk_mom_2,           &
     &      flex_data%i_drmax_v, nod_fld%d_fld, flex_data)
      end if
!
      if(flex_data%i_drmax_p .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous press'
        call check_scalar_evo_by_previous                               &
     &     (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,        &
     &      iphys%i_press, iphys%i_chk_press, iphys%i_chk_press_2,      &
     &      flex_data%i_drmax_p, nod_fld%d_fld, flex_data)
      end if
!
!
      if(flex_data%i_drmax_b .gt. izero) then
        if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          if(iflag_debug .gt. izero)                                    &
     &      write(*,*) 'check_vector_evo_by_previous vecp'
          call check_vector_evo_by_previous                             &
     &       (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,      &
     &        iphys%i_vecp, iphys%i_chk_uxb, iphys%i_chk_uxb_2,         &
     &        flex_data%i_drmax_b, nod_fld%d_fld, flex_data)
        else
          if(iflag_debug .gt. izero)                                    &
     &      write(*,*) 'check_vector_evo_by_previous magne'
          call check_vector_evo_by_previous                             &
     &       (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,      &
     &        iphys%i_magne, iphys%i_chk_uxb, iphys%i_chk_uxb_2,        &
     &        flex_data%i_drmax_b, nod_fld%d_fld, flex_data)
        end if
      end if
!
      if(flex_data%i_drmax_f .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous mag_p'
        call check_scalar_evo_by_previous                               &
     &     (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,        &
     &      iphys%i_mag_p, iphys%i_chk_potential,                       &
     &      iphys%i_chk_potential_2, flex_data%i_drmax_f,               &
     &      nod_fld%d_fld, flex_data)
      end if
!
!
      if(flex_data%i_drmax_t .gt. izero) then
        if(iflag_debug .gt. izero)                                      &
     &      write(*,*) 'check_scalar_evo_by_previous temp'
        call check_scalar_evo_by_previous                               &
     &     (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,        &
     &      iphys%i_temp, iphys%i_chk_heat, iphys%i_chk_heat_2,         &
     &      flex_data%i_drmax_t, nod_fld%d_fld, flex_data)
      end if
!
      if(flex_data%i_drmax_d .gt. izero) then
        call check_scalar_evo_by_previous                               &
     &     (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,        &
     &      iphys%i_light, iphys%i_chk_composit,                        &
     &      iphys%i_chk_composit_2, flex_data%i_drmax_d,                &
     &      nod_fld%d_fld, flex_data)
      end if
!$omp end parallel
!
      flex_data%d_ratio_max_l(1:flex_data%ntot_comp)                    &
     &        = flex_data%d_ratio_max_smp(1,1:flex_data%ntot_comp)
      flex_data%d_ratio_min_l(1:flex_data%ntot_comp)                    &
     &        = flex_data%d_ratio_min_smp(1,1:flex_data%ntot_comp)
      do nd = 1, flex_data%ntot_comp
        do ip = 2, np_smp
          flex_data%d_ratio_max_l(nd)                                   &
     &         = max(flex_data%d_ratio_max_l(nd),                       &
     &               flex_data%d_ratio_max_smp(ip,nd))
          flex_data%d_ratio_min_l(nd)                                   &
     &         = min(flex_data%d_ratio_min_l(nd),                       &
     &              flex_data%d_ratio_min_smp(ip,nd))
        end do
      end do
!
      call MPI_allREDUCE                                                &
     &   (flex_data%d_ratio_max_l, flex_data%d_ratio_max,               &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_MAX,                   &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE                                                &
     &   (flex_data%d_ratio_min_l, flex_data%d_ratio_min,               &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_MIN,                   &
     &    CALYPSO_COMM, ierr_MPI)
!
!
      flex_data%d_ratio_allmax = flex_data%d_ratio_max(1)
      flex_data%d_ratio_allmin = flex_data%d_ratio_min(1)
      do nd = 2, flex_data%ntot_comp
        flex_data%d_ratio_allmax                                        &
     &        = max(flex_data%d_ratio_allmax,flex_data%d_ratio_max(nd))
        flex_data%d_ratio_allmin                                        &
     &        = min(flex_data%d_ratio_allmin,flex_data%d_ratio_min(nd))
      end do
!
      end subroutine s_check_deltat_by_previous
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_scalar_evo_by_previous(numnod, inod_smp_stack,   &
     &         ncomp_nod, i_fld, i_chk, i_chk2, idrm, d_nod, flex_data)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_fld, i_chk, i_chk2, idrm
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      type(flexible_stepping_data), intent(inout) :: flex_data
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
          flex_data%d_ratio_min_smp(ip,idrm  ) =  1.0d30
          flex_data%d_ratio_max_smp(ip,idrm  ) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld  ) - d_nod(ist,i_chk2  ))      &
     &           / (two*(d_nod(ist,i_chk  ) - d_nod(ist,i_chk2  )) )
          flex_data%d_ratio_max_smp(ip,idrm  ) =  abs(d_ratio - one)
          flex_data%d_ratio_min_smp(ip,idrm  ) =  abs(d_ratio - one)
        end if
!
        do inod = ist+1, ied
          if( d_nod(inod,i_chk  ) .eq. d_nod(inod,i_chk2  )) then
            flex_data%d_ratio_max_smp(ip,idrm)                          &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm), zero)
            flex_data%d_ratio_min_smp(ip,idrm)                          &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm), 1.0d30)
          else
            d_ratio =    (d_nod(inod,i_fld  ) - d_nod(inod,i_chk2  ) )  &
     &            / (two*(d_nod(inod,i_chk  ) - d_nod(inod,i_chk2  ) ))
            d_ratio = abs(d_ratio - one)
!            write(100+my_rank,'(i16,1p4e16.5)') inod, d_ratio, d_nod(inod,i_fld  ), d_nod(inod,i_chk  ), d_nod(inod,i_chk2  )
            flex_data%d_ratio_max_smp(ip,idrm)                          &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm), d_ratio)
            flex_data%d_ratio_min_smp(ip,idrm)                          &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm), d_ratio)
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
     &         ncomp_nod, i_fld, i_chk, i_chk2, idrm, d_nod, flex_data)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_fld, i_chk, i_chk2, idrm
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      type(flexible_stepping_data), intent(inout) :: flex_data
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
          flex_data%d_ratio_min_smp(ip,idrm  ) =  1.0d30
          flex_data%d_ratio_max_smp(ip,idrm  ) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld  ) - d_nod(ist,i_chk2  ))      &
     &           / (two*(d_nod(ist,i_chk  ) - d_nod(ist,i_chk2  )) )
          flex_data%d_ratio_max_smp(ip,idrm  ) =  abs(d_ratio - one)
          flex_data%d_ratio_min_smp(ip,idrm  ) =  abs(d_ratio - one)
        end if
!
        if( d_nod(ist,i_chk+1) .eq. d_nod(ist,i_chk2+1)) then
          flex_data%d_ratio_min_smp(ip,idrm+1) =  1.0d30
          flex_data%d_ratio_max_smp(ip,idrm+1) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld+1) - d_nod(ist,i_chk2+1))      &
     &           / (two*(d_nod(ist,i_chk+1) - d_nod(ist,i_chk2+1)) )
          flex_data%d_ratio_max_smp(ip,idrm+1) =  abs(d_ratio - one)
          flex_data%d_ratio_min_smp(ip,idrm+1) =  abs(d_ratio - one)
        end if
!
        if( d_nod(ist,i_chk+2) .eq. d_nod(ist,i_chk2+2)) then
          flex_data%d_ratio_min_smp(ip,idrm+2) =  1.0d30
          flex_data%d_ratio_max_smp(ip,idrm+2) =  zero
        else
          d_ratio =     (d_nod(ist,i_fld+2) - d_nod(ist,i_chk2+2))      &
     &           / (two*(d_nod(ist,i_chk+2) - d_nod(ist,i_chk2+2)) )
          flex_data%d_ratio_max_smp(ip,idrm+2) =  abs(d_ratio - one)
          flex_data%d_ratio_min_smp(ip,idrm+2) =  abs(d_ratio - one)
        end if
!
        do inod = ist+1, ied
          if( d_nod(inod,i_chk  ) .eq. d_nod(inod,i_chk2  )) then
            flex_data%d_ratio_max_smp(ip,idrm)                          &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm), zero)
            flex_data%d_ratio_min_smp(ip,idrm)                          &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld  ) - d_nod(inod,i_chk2  ))    &
     &           / (two*(d_nod(inod,i_chk  ) - d_nod(inod,i_chk2  )) )
            d_ratio = abs(d_ratio - one)
            flex_data%d_ratio_max_smp(ip,idrm  )                        &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm  ),d_ratio)
            flex_data%d_ratio_min_smp(ip,idrm  )                        &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm  ),d_ratio)
          end if
!
          if( d_nod(inod,i_chk+1) .eq. d_nod(inod,i_chk2+1)) then
            flex_data%d_ratio_max_smp(ip,idrm+1)                        &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm+1), zero)
            flex_data%d_ratio_min_smp(ip,idrm+1)                        &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm+1), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld+1) - d_nod(inod,i_chk2+1))    &
     &           / (two*(d_nod(inod,i_chk+1) - d_nod(inod,i_chk2+1)) )
            d_ratio = abs(d_ratio - one)
            flex_data%d_ratio_max_smp(ip,idrm+1)                        &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm+1),d_ratio)
            flex_data%d_ratio_min_smp(ip,idrm+1)                        &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm+1),d_ratio)
          end if
!
          if( d_nod(inod,i_chk+2) .eq. d_nod(inod,i_chk2+2)) then
            flex_data%d_ratio_max_smp(ip,idrm+2)                        &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm+2), zero)
            flex_data%d_ratio_min_smp(ip,idrm+2)                        &
     &            = min(flex_data%d_ratio_min_smp(ip,idrm+2), 1.0d30)
          else
            d_ratio =   (d_nod(inod,i_fld+2) - d_nod(inod,i_chk2+2))    &
     &           / (two*(d_nod(inod,i_chk+2) - d_nod(inod,i_chk2+2)) )
            d_ratio = abs(d_ratio - one)
            flex_data%d_ratio_max_smp(ip,idrm+2)                        &
     &            = max(flex_data%d_ratio_max_smp(ip,idrm+2),d_ratio)
            flex_data%d_ratio_min_smp(ip,idrm+2)                        &
     &            = max(flex_data%d_ratio_min_smp(ip,idrm+2),d_ratio)
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
