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
      use single_const_surface_mesh
      use getarg_kemo
      use t_mesh_data
      use t_file_IO_parameter
      use t_mesh_data_4_merge
      use find_mesh_file_format
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
      type(field_IO_params), save :: mesh_file_viewer
      integer(kind = kint) :: num_pe_s
!
!
      icount = iargc_kemo()
      if(icount .eq. 0) then
        write(*,*) ' Please input header of mesh  !!'
        read (*,*) file_head
      else
        call getarg_k(1, file_head)
      end if
      mesh_file_viewer%file_prefix = file_head
!
      call find_mesh_format_4_viewer(mesh_file_viewer)
      call count_subdomains_4_viewer(mesh_file_viewer, num_pe_s)
      call choose_surface_mesh_sgl(num_pe_s, mesh_file_viewer)
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
