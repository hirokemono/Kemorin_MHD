!load_psf_data.f90
!      module load_psf_data
!
!      Written by H. Matsui
!
!      subroutine s_load_psf_data(istep)
!
      module load_psf_data
!
      use m_precision
      use m_psf_results
      use t_ucd_data
!
      implicit none
!
      private :: set_psf_udt_mesh, set_psf_udt_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_load_psf_data(istep)
!
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep
!
      type(ucd_data) :: read_psf
!
!
      read_psf%file_prefix = psf_file_header
      read_psf%ifmt_file =   iflag_psf_fmt
!
      call sel_read_ucd_file(-1, istep, ithree, read_psf)
!
      call set_psf_udt_mesh(read_psf)
      call set_psf_udt_data(read_psf)
!
      end subroutine s_load_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_mesh(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      numnod_psf =   ucd%nnod
      numele_psf =   ucd%nele
      ncomptot_psf = ucd%ntot_comp
      call allocate_psf_results
!
      inod_psf(1:numnod_psf) = ucd%inod_global(1:numnod_psf)
      iele_psf(1:numele_psf) = ucd%iele_global(1:numele_psf)
      xx_psf(1:numnod_psf,1:ithree) = ucd%xx(1:numnod_psf,1:ithree)
      ie_psf(1:numele_psf,1:ithree) = ucd%ie(1:numele_psf,1:ithree)
!
      call deallocate_ucd_node(ucd)
      call deallocate_ucd_ele(ucd)
!
      end subroutine set_psf_udt_mesh
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nfield_psf = ucd%num_field
      call allocate_psf_num_field
!
      psf_data_name(1:nfield_psf) =  ucd%phys_name
      ncomp_psf(1:nfield_psf) =      ucd%num_comp
      call count_stack_tot_psf_field
!
      call allocate_psf_field_data
      d_nod_psf(1:numnod_psf,1:ncomptot_psf)                            &
     &    = ucd%d_ucd(1:numnod_psf,1:ncomptot_psf)
!
      call deallocate_ucd_data(ucd)
!
      end subroutine set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      end module load_psf_data
