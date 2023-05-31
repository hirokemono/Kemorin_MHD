!>@file   collect_psf_data.f90
!!@brief  module collect_psf_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,&
!!     &                              d_img, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        integer(kind = kint) , intent(in) :: ifld_img
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: field
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_img(node%istack_internod(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: xx_out(node%istack_internod(nprocs),n_vector)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(element_data), intent(in) :: ele
!!        integer(kind = kinds), intent(inout)                          &
!!     &            :: ie_out(node%istack_internod(nprocs),num_triangle)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module collect_psf_data
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_mesh(num_psf, psf_mesh, psf_file_IO,        &
     &                           psf_dat, psf_out, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
!
      type(field_IO_params), intent(inout) :: psf_file_IO(num_psf)
      type(psf_results), intent(inout) :: psf_dat(num_psf)
      type(ucd_data), intent(inout) :: psf_out(num_psf)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
      integer :: irank_draw
!
!
      do i_psf = 1, num_psf
        irank_draw = mod(i_psf,nprocs)
        call merge_write_psf_mesh                                       &
     &     (irank_draw, psf_mesh(i_psf), psf_file_IO(i_psf),            &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_ele,             &
     &      psf_out(i_psf), SR_sig)
      end do
!
      end subroutine output_map_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_file(num_psf, psf_file_IO, istep_psf,       &
     &          time_d, psf_mesh, t_IO, psf_dat, psf_out, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
      type(field_IO_params), intent(in) :: psf_file_IO(num_psf)
!
      type(time_data), intent(inout) :: t_IO
      type(psf_results), intent(inout) :: psf_dat(num_psf)
      type(ucd_data), intent(inout) :: psf_out(num_psf)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
      integer :: irank_draw
!
!
      call copy_time_step_size_data(time_d, t_IO)
!
      do i_psf = 1, num_psf
        irank_draw = mod(i_psf,nprocs)
        call merge_write_psf_file(irank_draw, istep_psf,                &
     &      psf_file_IO(i_psf), psf_mesh(i_psf), t_IO,                  &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_phys,            &
     &      psf_out(i_psf), SR_sig)
      end do
!
      end subroutine output_map_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_mesh(irank_draw, psf_mesh,             &
     &          psf_file_IO, psf_nod, psf_ele, psf_ucd, SR_sig)
!
      use t_psf_patch_data
      use t_ucd_data
      use t_file_IO_parameter
!
      use cal_mesh_position
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer, intent(in) :: irank_draw
      type(psf_local_data), intent(in) :: psf_mesh
      type(field_IO_params), intent(inout) :: psf_file_IO
      type(node_data), intent(inout) ::    psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(ucd_data), intent(inout) ::     psf_ucd
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: i
!
      psf_nod%numnod = 0
      psf_ele%numele = 0
      psf_ele%nnod_4_ele = psf_mesh%patch%nnod_4_ele
      if(my_rank .eq. irank_draw) then
        psf_nod%numnod = int(psf_mesh%node%istack_internod(nprocs))
        psf_ele%numele = int(psf_mesh%patch%istack_interele(nprocs))
      end if
      psf_nod%internal_node = psf_nod%numnod
!
      call alloc_node_geometry_w_sph(psf_nod)
      call collect_psf_node(irank_draw, psf_mesh%node,                  &
     &                      psf_nod%xx, SR_sig)
      call set_spherical_position(psf_nod)
      call calypso_mpi_barrier
!
!$omp parallel do
      do i = 1, psf_nod%numnod
        psf_nod%inod_global(i) = i
      end do
!$omp end parallel do
!
!
      call alloc_element_types(psf_ele)
      psf_ele%first_ele_type = psf_mesh%patch%first_ele_type
!$omp parallel do
      do i = 1, psf_ele%numele
        psf_ele%iele_global(i) = i
        psf_ele%elmtyp(i) = psf_mesh%patch%elmtyp(1)
        psf_ele%nodelm(i) = psf_mesh%patch%nodelm(1)
      end do
!$omp end parallel do
!
      call alloc_ele_connectivity(psf_ele)
      call collect_psf_element(irank_draw, psf_mesh%patch,              &
     &                         psf_ele%ie, SR_sig)
      call calypso_mpi_barrier
!
      if(my_rank .eq. irank_draw) then
        call link_node_data_2_ucd(psf_nod, psf_ucd)
        call link_ele_data_2_ucd(psf_ele, psf_ucd)
!
        if(psf_file_IO%iflag_format .gt. iflag_single) then
          psf_file_IO%iflag_format = psf_file_IO%iflag_format           &
     &                              - iflag_single
        end if
        call sel_write_grd_file(-1, psf_file_IO, psf_ucd)
      end if
!      call dealloc_ele_connect(psf_ele)
!      call dealloc_node_geometry_w_sph(psf_nod)
!
      end subroutine merge_write_psf_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_file(irank_draw, istep_psf,            &
     &          psf_file_IO, psf_mesh, t_IO, psf_nod, psf_phys,         &
     &          psf_ucd, SR_sig)
!
      use t_psf_patch_data
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      use append_phys_data
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer, intent(in) :: irank_draw
      integer(kind = kint), intent(in) :: istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(field_IO_params), intent(in) :: psf_file_IO
      type(time_data), intent(in) :: t_IO
!
      type(phys_data), intent(inout) :: psf_phys
      type(node_data), intent(inout) :: psf_nod
      type(ucd_data), intent(inout) :: psf_ucd
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: i_img
!
!
      psf_nod%numnod = 0
      psf_phys%ntot_phys = psf_mesh%field%ntot_phys
      if(my_rank .eq. irank_draw) then
        psf_nod%numnod = int(psf_mesh%node%istack_internod(nprocs))
      end if
!
      call copy_field_name(psf_mesh%field, psf_phys)
      call alloc_phys_data(psf_nod%numnod, psf_phys)
!
      do i_img = 1, psf_phys%ntot_phys
        call collect_psf_scalar(irank_draw, i_img, psf_mesh%node,       &
     &                          psf_mesh%field, psf_phys%d_fld, SR_sig)
        call calypso_mpi_barrier
      end do
!
      if(my_rank .eq. irank_draw) then
        call link_field_data_to_ucd(psf_phys, psf_ucd)
        call sel_write_ucd_file                                         &
     &     (-1, istep_psf, psf_file_IO, t_IO, psf_ucd)
      end if
!
      call dealloc_phys_data(psf_phys)
      call dealloc_phys_name(psf_phys)
!
      end subroutine merge_write_psf_file
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,  &
     &                              d_img, SR_sig)
!
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      integer(kind = kint) , intent(in) :: ifld_img
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: field
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_img(node%istack_internod(nprocs))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nnod
!
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      call collect_send_recv_N(irank_draw, n_scalar, nnod,              &
     &                         field%d_fld(1,ifld_img),                 &
     &                         node%istack_internod, d_img(1), SR_sig)
!
      end subroutine collect_psf_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!
      use m_phys_constants
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      type(node_data), intent(in) :: node
!
      real(kind = kreal), intent(inout)                                 &
     &           :: xx_out(node%istack_internod(nprocs),n_vector)
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      integer(kind = kint) :: nnod, nd
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      do nd = 1, n_vector
        call collect_send_recv_N(irank_draw, ione, nnod,                &
     &                           node%xx(1,nd), node%istack_internod,   &
     &                           xx_out(1,nd), SR_sig)
        call calypso_mpi_barrier
      end do
!
      end subroutine collect_psf_node
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!
      use m_geometry_constants
      use t_solver_SR_int
      use collect_SR_int
!
      integer, intent(in) :: irank_draw
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(inout)                               &
     &              :: ie_out(ele%istack_interele(nprocs),num_triangle)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nele, nd
!
!
      nele = int(ele%istack_interele(my_rank+1)                         &
     &           - ele%istack_interele(my_rank))
      do nd = 1, num_triangle
        call collect_send_recv_int(irank_draw, ione, nele,              &
     &                             ele%ie(1,nd), ele%istack_interele,   &
     &                             ie_out(1,nd), SR_sig)
        call calypso_mpi_barrier
      end do
!
      end subroutine collect_psf_element
!
!  ---------------------------------------------------------------------
!
      end module collect_psf_data
