!copy_pick_udt_data_plane.f90
!      module copy_pick_udt_data_plane
!
!!      subroutine init_by_ucd_4_plane_model                            &
!!     &         (istep, ucd_param, nod_fld, t_IO, ucd)
!!      subroutine read_udt_data_4_plane_model(num_pe, istep,           &
!!     &          nnod_target, nfield_target, icomp_target,             &
!!     &          ifield_target, phys_data, nnod_max, mesh,             &
!!     &          ucd_param, t_IO, ucd)
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!        type(phys_data), intent(inout) :: nod_fld
!
!      written by H. Matsui
!
      module copy_pick_udt_data_plane
!
      use m_precision
!
      implicit none
!
      private :: copy_and_pick_ucd_data_merge
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_by_ucd_4_plane_model                              &
     &         (istep, ucd_param, nod_fld, t_IO, ucd)
!
      use m_file_format_switch
      use t_file_IO_parameter
      use t_phys_data
      use t_time_data
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: nod_fld
      integer (kind = kint) :: i
!
!
      ucd%nnod = ione
      call sel_read_udt_param(izero, istep, ucd_param, t_IO, ucd)
      call deallocate_ucd_phys_data(ucd)
!
      nod_fld%num_phys =    ucd%num_field
      call alloc_phys_name_type(nod_fld)
!
      nod_fld%istack_component(0) = 0
      do i = 1, nod_fld%num_phys
        nod_fld%phys_name(i) =      ucd%phys_name(i)
        nod_fld%num_component(i) =  ucd%num_comp(i)
        nod_fld%istack_component(i) = nod_fld%istack_component(i-1)     &
     &                              + nod_fld%num_component(i)
      end do
!
      end subroutine init_by_ucd_4_plane_model
!
! -----------------------------------------------------------------------
!
      subroutine read_udt_data_4_plane_model(num_pe, istep,             &
     &          nnod_target, nfield_target, icomp_target,               &
     &          ifield_target, phys_data, nnod_max, mesh,               &
     &          ucd_param, t_IO, ucd)
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_time_data
      use t_ucd_data
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: istep, nnod_max
      type(mesh_geometry), intent(in) :: mesh(num_pe)
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint), intent(in) :: nfield_target, nnod_target
      integer(kind = kint), intent(in) :: icomp_target(nfield_target)
      integer(kind = kint), intent(in) :: ifield_target(nfield_target)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: phys_data(nnod_target*nfield_target)
!
      integer :: ip, id_rank
!
!
      ucd%nnod = nnod_max
      call allocate_ucd_phys_data(ucd)
      do ip =1, num_pe
        id_rank = ip - 1
!
        ucd%nnod =        mesh(ip)%node%numnod
        call sel_read_udt_file(id_rank, istep, ucd_param, t_IO, ucd)
!
        call copy_and_pick_ucd_data_merge                               &
     &         (nnod_target, nfield_target, icomp_target,               &
     &          ifield_target, phys_data, mesh(ip), ucd)
      end do
      call deallocate_ucd_phys_data(ucd)
!
      end subroutine read_udt_data_4_plane_model
!
! -----------------------------------------------------------------------
!
      subroutine copy_and_pick_ucd_data_merge                           &
     &         (nnod_target, nfield_target, icomp_target,               &
     &          ifield_target, phys_data, mesh, ucd)
!
      use t_mesh_data
      use t_ucd_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(ucd_data), intent(in) :: ucd
!
      integer(kind = kint), intent(in) :: nfield_target, nnod_target
      integer(kind = kint), intent(in) :: icomp_target(nfield_target)
      integer(kind = kint), intent(in) :: ifield_target(nfield_target)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: phys_data(nnod_target*nfield_target)
!
      integer(kind = kint) :: j, ic
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: inod_gl, ii
!
!
      do inod =1, mesh%node%internal_node
        inod_gl = mesh%node%inod_global(inod)
!
        if (int(inod_gl) .le. nnod_target) then
          ic  = 0
          do j = 1, nfield_target
            if ( icomp_target(j) .ge. 0) then
              ic = icomp_target(j) + ifield_target(j)
              ii = inod_gl + (j-1)*nnod_target
!
              phys_data(ii) = ucd%d_ucd(inod,ic)
            end if
          end do
!
        end if
      end do
!
      end subroutine copy_and_pick_ucd_data_merge
!
! -----------------------------------------------------------------------
!
      end module copy_pick_udt_data_plane
