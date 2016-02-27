!
!     program  kemoviewer_f
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program   kemoviewer_f
!
      use m_precision
!
      use const_surface_mesh
      use getarg_kemo
      use t_geometry_data
!
      implicit    none
!
!    generate surface mesh
!
      integer(kind = kint), parameter :: iflag_draw_mesh = 1
      integer(kind = kint) :: icount
      character(len=kchara) :: file_head
      character(len = 1) :: chara_flag
!
      type(element_data), save :: ele_viewer
!
      icount = iargc_kemo()
      if(icount .eq. 0) then
        write(*,*) ' Please input header of mesh  !!'
        read (*,*) file_head
      else
        call getarg_k(1, file_head)
      end if
!
      call choose_surface_mesh(file_head, ele_viewer, surf1, edge1)
!
      write(*,*) 'will you draw mesh? (y/n)'
      read(*,*) chara_flag
      if (chara_flag.eq.'n' .or. chara_flag.eq.'N')  then
        stop ' //// program normally finished //// '
      end if
!
      call draw_mesh_kemo(%VAL(0), %VAL(iflag_draw_mesh))
!
      stop ' //// program normally finished //// '
!
!
      end program kemoviewer_f
