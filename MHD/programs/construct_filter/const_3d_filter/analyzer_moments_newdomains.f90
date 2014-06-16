!analyzer_moments_newdomains.f90
!      module analyzer_moments_newdomains
!..................................................
!
!      modified by H. Matsui on Feb., 2010
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_moments_newdomains
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_2nd_pallalel_vector
      use t_mesh_data
!
      implicit none
!
      type(mesh_geometry), save ::    newmesh
      type(surface_geometry), save :: new_surf_mesh
      type(edge_geometry), save ::    new_edge_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_ctl_data_newdomain_filter
      use m_ctl_param_newdom_filter
      use const_domain_tbl_by_file
!
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
      call copy_num_processes_to_2nd
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain(ierr)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_ctl_data_newdomain_filter
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_para'
      if (iflag_set_filter_elen .gt. 0                                  &
     &  .or. iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_para(newmesh,                    &
     &      new_surf_mesh, new_edge_mesh)
      end if
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_moments_newdomains
