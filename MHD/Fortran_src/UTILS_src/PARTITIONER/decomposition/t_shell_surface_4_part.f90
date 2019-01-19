!
!      module t_shell_surface_4_part
!
!     Written by H. Matsui
!
!
!!      subroutine alloc_xyz_cmb_4_part(sphere_4_part)
!!      subroutine alloc_item_sph_4_part(sphere_4_part)
!!      subroutine alloc_IGROUP_radius(sphere_4_part)
!!      subroutine alloc_IGROUP_CMB(sphere_4_part)
!!      subroutine alloc_quad_shell(sphere_4_part)
!!      subroutine alloc_local_nnod_shell(nproc, sphere_4_part)
!!        type(shell_surface_4_part), intent(inout) :: sphere_4_part
!!
!!      subroutine dealloc_linear_shell(sphere_4_part)
!!      subroutine dealloc_quad_shell(sphere_4_part)
!!      subroutine dealloc_local_nnod_shell(sphere_4_part)
!!        type(shell_surface_4_part), intent(inout) :: sphere_4_part
!!
!!      subroutine read_surface_connect_linear(file_name, sphere_4_part)
!!      subroutine read_surface_connect_quad(file_name, sphere_4_part)
!!        type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      module t_shell_surface_4_part
!
      use m_precision
!
      implicit none
!
      type shell_surface_4_part
        integer(kind=kint ) :: nnod_CMB
        integer(kind=kint ) :: num_CMB
        integer(kind=kint ) :: nedge_CMB
        integer(kind=kint ) :: nele_CMB
        integer(kind=kint ) :: num_layer
        integer(kind=kint ) :: nlayer_ICB
        integer(kind=kint ) :: nlayer_CMB
        integer(kind=kint ) :: nnod_cube
        integer(kind=kint ) :: num_cube
        integer(kind=kint ) :: nedge_cube
        integer(kind=kint ) :: nunod_20_CMB
!
        real(kind=kreal),  allocatable :: xx_cmb(:,:)
        real(kind=kreal),  allocatable :: rtp_cmb(:,:)
        real(kind=kreal),  allocatable :: ar_cmb(:)
        real(kind=kreal),  allocatable :: s_cmb(:)
        real(kind=kreal),  allocatable :: as_cmb(:)

        integer(kind=kint),  allocatable :: inod_free(:)

        integer(kind=kint ),  allocatable :: istack_sph(:)
        integer(kind=kint ),  allocatable :: item_sph(:,:)
        integer(kind=kint ),  allocatable :: istack20_sph(:)
        integer(kind=kint ),  allocatable :: item20_sph(:,:)

        integer(kind=kint ), dimension(:), allocatable :: numnod_local
        integer(kind=kint ), dimension(:), allocatable :: ncore_local
        integer(kind=kint ), dimension(:), allocatable :: nrest_local
!
        integer(kind=kint ), dimension(:), allocatable :: numcmb_local

        integer(kind=kint ),  allocatable :: IGROUP_cmb(:)
        integer(kind=kint ),  allocatable :: IGROUP_radius(:)
      end type shell_surface_4_part
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_xyz_cmb_4_part(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
!
      allocate ( sphere_4_part%rtp_cmb(sphere_4_part%num_CMB,3) )
      allocate ( sphere_4_part%istack_sph(sphere_4_part%nnod_CMB) )
      allocate ( sphere_4_part%xx_cmb(sphere_4_part%num_CMB,3) )
      allocate ( sphere_4_part%ar_cmb(sphere_4_part%num_CMB) )
      allocate ( sphere_4_part%s_cmb(sphere_4_part%num_CMB) )
      allocate ( sphere_4_part%as_cmb(sphere_4_part%num_CMB) )
!
      if(sphere_4_part%num_CMB .gt. 0) then
        sphere_4_part%rtp_cmb = 0.0d0
        sphere_4_part%xx_cmb = 0.0d0
        sphere_4_part%ar_cmb = 0.0d0
        sphere_4_part%s_cmb = 0.0d0
        sphere_4_part%as_cmb = 0.0d0
!
        sphere_4_part%istack_sph = 0
      end if
!
      end subroutine alloc_xyz_cmb_4_part
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_item_sph_4_part(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
!
      allocate ( sphere_4_part%item_sph(sphere_4_part%num_layer,        &
     &                                  sphere_4_part%nnod_CMB) )
      allocate ( sphere_4_part%inod_free(sphere_4_part%num_cube))
!
      if(sphere_4_part%num_CMB .gt. 0) then
        if(sphere_4_part%num_layer.gt.0) sphere_4_part%item_sph = 0
      end if
      if(sphere_4_part%num_cube.gt.0) sphere_4_part%inod_free = 0
!
      end subroutine alloc_item_sph_4_part
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_IGROUP_radius(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
!
      allocate ( sphere_4_part%IGROUP_radius(sphere_4_part%num_layer) )
      if(sphere_4_part%num_layer.gt.0) sphere_4_part%IGROUP_radius = 0
!
      end subroutine alloc_IGROUP_radius
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_IGROUP_CMB(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
!
      allocate ( sphere_4_part%IGROUP_cmb(sphere_4_part%num_CMB) )
      if(sphere_4_part%num_CMB .gt. 0)  sphere_4_part%IGROUP_cmb = 0
!
      end subroutine alloc_IGROUP_CMB
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_quad_shell(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      allocate ( sphere_4_part%istack20_sph(sphere_4_part%num_CMB) )
      allocate ( sphere_4_part%item20_sph(sphere_4_part%num_layer,      &
     &                                    sphere_4_part%num_CMB) )
!
      if(sphere_4_part%num_CMB .gt. 0) then
        sphere_4_part%istack20_sph = 0
        if(sphere_4_part%num_layer.gt.0) sphere_4_part%item20_sph = 0
      end if
!
      end subroutine alloc_quad_shell
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_nnod_shell(nproc, sphere_4_part)
!
      integer(kind = kint), intent(in) :: nproc
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
!
      allocate (sphere_4_part%numnod_local(nproc))
      allocate (sphere_4_part%numcmb_local(nproc))
      allocate (sphere_4_part%ncore_local (nproc))
      allocate (sphere_4_part%nrest_local (nproc))
!
      if(nproc .le. 0) return
      sphere_4_part%numnod_local = 0
      sphere_4_part%numcmb_local = 0
      sphere_4_part%ncore_local =  0
      sphere_4_part%nrest_local =  0
!
      end subroutine alloc_local_nnod_shell
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_linear_shell(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      deallocate ( sphere_4_part%xx_cmb )
      deallocate ( sphere_4_part%rtp_cmb )
      deallocate ( sphere_4_part%ar_cmb )
      deallocate ( sphere_4_part%s_cmb )
      deallocate ( sphere_4_part%as_cmb )
!
      deallocate ( sphere_4_part%IGROUP_cmb )
      deallocate ( sphere_4_part%IGROUP_radius )
!
      deallocate ( sphere_4_part%istack_sph )
      deallocate ( sphere_4_part%item_sph )
      deallocate (sphere_4_part%inod_free )
!
      end subroutine dealloc_linear_shell
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_quad_shell(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      deallocate ( sphere_4_part%istack20_sph )
      deallocate ( sphere_4_part%item20_sph )
!
      end subroutine dealloc_quad_shell
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_nnod_shell(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      deallocate (sphere_4_part%numnod_local)
      deallocate (sphere_4_part%numcmb_local)
      deallocate (sphere_4_part%ncore_local )
      deallocate (sphere_4_part%nrest_local )
!
      end subroutine dealloc_local_nnod_shell
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_surface_connect_linear(file_name, sphere_4_part)
!
      character(len=kchara), intent(in) :: file_name
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmp_character
      integer(kind = kint) :: itmp
!
!
      open (21,file=file_name)
!
      read(21,*) tmp_character
      read(21,*) sphere_4_part%nnod_cube, sphere_4_part%nedge_cube
!
      read(21,*) tmp_character
      read(21,*) sphere_4_part%nnod_CMB, sphere_4_part%nedge_CMB,       &
     &          sphere_4_part%num_layer
!
      read(21,*) tmp_character
      read(21,*) sphere_4_part%nele_CMB
!
      read(21,*) tmp_character
      read(21,*) sphere_4_part%nlayer_ICB, sphere_4_part%nlayer_CMB
!
      sphere_4_part%num_cube = sphere_4_part%nnod_cube
      sphere_4_part%num_CMB = sphere_4_part%nnod_CMB
!
      call alloc_xyz_cmb_4_part(sphere_4_part)
      call alloc_item_sph_4_part(sphere_4_part)
      call alloc_IGROUP_CMB(sphere_4_part)
      call alloc_IGROUP_radius(sphere_4_part)
!
      read(21,*) tmp_character
      do i = 1, sphere_4_part%num_CMB
        read(21,*) itmp, sphere_4_part%xx_cmb(i,1:3)
      end do
!
      close(21)
!
      end subroutine read_surface_connect_linear
!
!  ---------------------------------------------------------------------
!
      subroutine read_surface_connect_quad(file_name, sphere_4_part)
!
      character(len=kchara), intent(in) :: file_name
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmp_character
      integer(kind = kint) :: itmp
!
!
       open (21,file=file_name)
!
       read(21,*) tmp_character
       read(21,*) sphere_4_part%nnod_cube, sphere_4_part%nedge_cube
!
       read(21,*) tmp_character
       read(21,*) sphere_4_part%nnod_CMB, sphere_4_part%nedge_CMB,      &
     &           sphere_4_part%num_layer
!
       read(21,*) tmp_character
       read(21,*) sphere_4_part%nele_CMB
!
       read(21,*) tmp_character
       read(21,*) sphere_4_part%nlayer_ICB, sphere_4_part%nlayer_CMB
!
       sphere_4_part%num_cube                                           &
     &      = sphere_4_part%nnod_cube + sphere_4_part%nedge_cube
       sphere_4_part%num_CMB                                            &
     &      = sphere_4_part%nnod_CMB + sphere_4_part%nedge_CMB
!
      call alloc_xyz_cmb_4_part(sphere_4_part)
      call alloc_item_sph_4_part(sphere_4_part)
      call alloc_IGROUP_CMB(sphere_4_part)
      call alloc_IGROUP_radius(sphere_4_part)
!
      call alloc_quad_shell(sphere_4_part)
!
      read(21,*) tmp_character
      do i = 1, sphere_4_part%num_CMB
        read(21,*) itmp, sphere_4_part%xx_cmb(i,1:3)
      end do
!
      close(21)
!
      end subroutine read_surface_connect_quad
!
!  ---------------------------------------------------------------------
!
      end module t_shell_surface_4_part
