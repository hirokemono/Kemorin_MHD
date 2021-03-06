!analyzer_moments_newdomains.f90
!      module analyzer_moments_newdomains
!..................................................
!
!      modified by H. Matsui on Feb., 2010
!
!      subroutine moments_to_newdomain_init
!      subroutine moments_to_newdomain_analyze
!
      module analyzer_moments_newdomains
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use t_mesh_data
      use t_ctl_param_newdom_filter
!
      implicit none
!
      integer, save :: nprocs_2nd
      type(ctl_param_newdom_filter), save :: newfil_p1
!
      type(mesh_geometry), save ::    orgmesh
      type(mesh_geometry), save ::    newmesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine moments_to_newdomain_init
!
      use t_ctl_data_newdomain_filter
      use t_ctl_param_newdom_filter
      use const_domain_tbl_by_file
!
      type(ctl_data_newdomain_filter) :: newd_fil_ctl1
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Transfer filtering table to another decomposition'
        write(*,*) 'Required file: '
        write(*,*) 'mesh data for filter function'
        write(*,*) 'mesh data for new gridds'
        write(*,*) 'filter coefficients data on original decomposition'
        write(*,*) 'filter moments data on original decomposition'
        write(*,*) 'element size data on new decomposition'
      end if
!
!     --------------------- 
!
      nprocs_2nd = nprocs
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain(newd_fil_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain                                 &
     &   (nprocs_2nd, newd_fil_ctl1%org_filter_plt,                     &
     &    newd_fil_ctl1%new_filter_plt, newd_fil_ctl1%ffile_ndom_ctl,   &
     &    newd_fil_ctl1%org_filter_file_ctls, newfil_p1, ierr)
!
      end subroutine moments_to_newdomain_init
!
! ----------------------------------------------------------------------
!
      subroutine moments_to_newdomain_analyze
!
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_para'
      if (newfil_p1%iflag_set_filter_elen .gt. 0                        &
     &  .or. newfil_p1%iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_para                             &
     &     (newfil_p1, orgmesh, newmesh)
      end if
!
      end subroutine moments_to_newdomain_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_moments_newdomains
