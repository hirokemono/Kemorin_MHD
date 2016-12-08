!
!     program  pick_surface
!
      program   pick_surface
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use m_read_mesh_data
      use const_surface_mesh
      use getarg_kemo
!
      implicit    none
!
      character(len=kchara) :: file_head
      integer(kind = kint) :: icount
!
      type(element_data), save :: ele_pick
      type(surface_data), save :: surf_pick
      type(edge_data), save :: edge_pick
!
      icount = iargc_kemo()
      if(icount .eq. 0) then
        write(*,*) ' Please input header of mesh  !!'
        read (*,*) file_head
      else
        call getarg_k(1, file_head)
      end if
!
      mesh1_file%file_prefix =    file_head
      call choose_surface_mesh                                          &
     &   (mesh1_file, ele_pick, surf_pick, edge_pick)
!
      stop ' //// program normally finished //// '
!
      end program pick_surface 
